import Foundation
import AsyncHTTPClient
import NIOHTTP1
import NIO
import NIOFoundationCompat
import Logging

enum FirestoreError: Error {
    case invalidUrl
    case apiError(status: HTTPResponseStatus, message: String)
    case decodingError(Error)
    case missingData
}

actor FirestoreClient {
    private let auth: GoogleTokenProvider
    private let httpClient: HTTPClient
    private let logger: Logger
    
    private var baseUrl: String {
        "https://firestore.googleapis.com/v1/projects/\(auth.projectId)/databases/(default)/documents"
    }
    
    init(auth: GoogleTokenProvider, httpClient: HTTPClient, logger: Logger) {
        self.auth = auth
        self.httpClient = httpClient
        self.logger = logger
    }
    
    private func getHeaders() async throws -> HTTPHeaders {
        let token = try await auth.getAccessToken()
        var headers = HTTPHeaders()
        headers.add(name: "Authorization", value: "Bearer \(token)")
        headers.add(name: "Content-Type", value: "application/json")
        return headers
    }
    
    // MARK: - CRUD
    
    func listProducts() async throws -> [Product] {
        let url = "\(baseUrl)/inventory"
        var request = HTTPClientRequest(url: url)
        request.method = .GET
        request.headers = try await getHeaders()
        
        let response = try await httpClient.execute(request, timeout: .seconds(30))
        
        guard response.status == .ok else {
            let body = try await response.body.collect(upTo: 1024 * 1024)
            throw FirestoreError.apiError(status: response.status, message: String(buffer: body))
        }
        
        let body = try await response.body.collect(upTo: 10 * 1024 * 1024)
        
        struct ListResponse: Codable {
            let documents: [FirestoreDocument]?
        }
        
        let listResponse = try JSONDecoder().decode(ListResponse.self, from: Data(buffer: body))
        return listResponse.documents?.compactMap { Product.from(document: $0) } ?? []
    }
    
    func getProduct(id: String) async throws -> Product? {
        let url = "\(baseUrl)/inventory/\(id)"
        var request = HTTPClientRequest(url: url)
        request.method = .GET
        request.headers = try await getHeaders()
        
        let response = try await httpClient.execute(request, timeout: .seconds(10))
        
        if response.status == .notFound {
            return nil
        }
        
        guard response.status == .ok else {
            let body = try await response.body.collect(upTo: 1024 * 1024)
            throw FirestoreError.apiError(status: response.status, message: String(buffer: body))
        }
        
        let body = try await response.body.collect(upTo: 1024 * 1024)
        let doc = try JSONDecoder().decode(FirestoreDocument.self, from: Data(buffer: body))
        return Product.from(document: doc)
    }
    
    func addProduct(_ product: Product) async throws {
        let url = "\(baseUrl)/inventory"
        var request = HTTPClientRequest(url: url)
        request.method = .POST
        request.headers = try await getHeaders()
        
        let doc = FirestoreDocument(fields: product.toFirestoreFields())
        let bodyData = try JSONEncoder().encode(doc)
        request.body = .bytes(ByteBuffer(bytes: Array(bodyData)))
        
        let response = try await httpClient.execute(request, timeout: .seconds(10))
        
        guard response.status == .ok else {
            let body = try await response.body.collect(upTo: 1024 * 1024)
            throw FirestoreError.apiError(status: response.status, message: String(buffer: body))
        }
    }
    
    func updateProduct(id: String, product: Product) async throws {
        let url = "\(baseUrl)/inventory/\(id)"
        var request = HTTPClientRequest(url: url)
        request.method = .PATCH
        request.headers = try await getHeaders()
        
        let doc = FirestoreDocument(fields: product.toFirestoreFields())
        let bodyData = try JSONEncoder().encode(doc)
        request.body = .bytes(ByteBuffer(bytes: Array(bodyData)))
        
        let response = try await httpClient.execute(request, timeout: .seconds(10))
        
        guard response.status == .ok else {
            let body = try await response.body.collect(upTo: 1024 * 1024)
            throw FirestoreError.apiError(status: response.status, message: String(buffer: body))
        }
    }
    
    func deleteProduct(id: String) async throws {
        let url = "\(baseUrl)/inventory/\(id)"
        var request = HTTPClientRequest(url: url)
        request.method = .DELETE
        request.headers = try await getHeaders()
        
        let response = try await httpClient.execute(request, timeout: .seconds(10))
        
        guard response.status == .ok else {
            let body = try await response.body.collect(upTo: 1024 * 1024)
            throw FirestoreError.apiError(status: response.status, message: String(buffer: body))
        }
    }
    
    // MARK: - Query
    
    func findProducts(name: String) async throws -> [Product] {
        let url = "\(baseUrl):runQuery"
        var request = HTTPClientRequest(url: url)
        request.method = .POST
        request.headers = try await getHeaders()
        
        // Construct StructuredQuery
        let query: [String: Any] = [
            "structuredQuery": [
                "from": [["collectionId": "inventory"]],
                "where": [
                    "fieldFilter": [
                        "field": ["fieldPath": "name"],
                        "op": "EQUAL",
                        "value": ["stringValue": name]
                    ]
                ]
            ]
        ]
        
        let bodyData = try JSONSerialization.data(withJSONObject: query)
        request.body = .bytes(ByteBuffer(bytes: Array(bodyData)))
        
        let response = try await httpClient.execute(request, timeout: .seconds(10))
        
        guard response.status == .ok else {
            let body = try await response.body.collect(upTo: 1024 * 1024)
            throw FirestoreError.apiError(status: response.status, message: String(buffer: body))
        }
        
        let body = try await response.body.collect(upTo: 10 * 1024 * 1024)
        
        struct QueryResponseItem: Codable {
            let document: FirestoreDocument?
            let readTime: String?
        }
        
        let items = try JSONDecoder().decode([QueryResponseItem].self, from: Data(buffer: body))
        return items.compactMap { $0.document }.compactMap { Product.from(document: $0) }
    }
    
    func queryProductsByName(name: String) async throws -> [Product] {
        let all = try await listProducts()
        return all.filter { $0.name.lowercased().contains(name.lowercased()) }
    }
    
    // MARK: - Batch
    
    func batchDelete(ids: [String]) async throws {
        if ids.isEmpty { return }
        
        let url = "https://firestore.googleapis.com/v1/projects/\(auth.projectId)/databases/(default)/documents:commit"
        var request = HTTPClientRequest(url: url)
        request.method = .POST
        request.headers = try await getHeaders()
        
        // We capture projectId in a local variable to use in the map closure safely if needed,
        // but since auth.projectId is nonisolated now, we can access it.
        // However, map closure is synchronous and non-isolated.
        let projectId = auth.projectId
        
        let writes = ids.map { id in
            return [
                "delete": "projects/\(projectId)/databases/(default)/documents/inventory/\(id)"
            ]
        }
        
        let body: [String: Any] = ["writes": writes]
        let bodyData = try JSONSerialization.data(withJSONObject: body)
        request.body = .bytes(ByteBuffer(bytes: Array(bodyData)))
        
        let response = try await httpClient.execute(request, timeout: .seconds(30))
        
        guard response.status == .ok else {
            let body = try await response.body.collect(upTo: 1024 * 1024)
            throw FirestoreError.apiError(status: response.status, message: String(buffer: body))
        }
    }
}