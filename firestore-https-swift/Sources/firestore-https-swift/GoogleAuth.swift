import Foundation
import JWTKit
import AsyncHTTPClient
import NIOHTTP1
import Logging
import NIO
import NIOFoundationCompat

struct ServiceAccount: Codable {
    let type: String
    let project_id: String?
    let quota_project_id: String?
    let private_key_id: String?
    let private_key: String?
    let client_email: String?
    let client_id: String?
    let client_secret: String?
    let refresh_token: String?
    let auth_uri: String?
    let token_uri: String?
    let auth_provider_x509_cert_url: String?
    let client_x509_cert_url: String?
}

struct GoogleClaims: JWTPayload {
    var iss: String
    var scope: String
    var aud: String
    var exp: ExpirationClaim
    var iat: IssuedAtClaim
    
    func verify(using signer: JWTSigner) throws {
        try exp.verifyNotExpired()
    }
}

struct TokenResponse: Codable {
    let access_token: String
    let expires_in: Int
    let token_type: String
}

actor GoogleTokenProvider {
    private let serviceAccount: ServiceAccount
    private let httpClient: HTTPClient
    private var logger: Logger
    
    private var currentToken: String?
    private var tokenExpiration: Date?
    
    init(serviceAccount: ServiceAccount, httpClient: HTTPClient, logger: Logger) {
        self.serviceAccount = serviceAccount
        self.httpClient = httpClient
        self.logger = logger
    }
    
    func getAccessToken() async throws -> String {
        if let token = currentToken, let expiration = tokenExpiration, expiration > Date() {
            return token
        }
        
        return try await refreshToken()
    }
    
    private func refreshToken() async throws -> String {
        if serviceAccount.type == "service_account" {
            return try await refreshServiceAccountToken()
        } else if serviceAccount.type == "authorized_user" {
            return try await refreshAuthorizedUserToken()
        } else {
            throw NSError(domain: "GoogleAuth", code: 400, userInfo: [NSLocalizedDescriptionKey: "Unsupported credential type: \(serviceAccount.type)"])
        }
    }
    
    private func refreshServiceAccountToken() async throws -> String {
        logger.debug("Refreshing Google Service Account Token...")
        
        guard let privateKey = serviceAccount.private_key,
              let clientEmail = serviceAccount.client_email,
              let tokenUri = serviceAccount.token_uri else {
            throw NSError(domain: "GoogleAuth", code: 400, userInfo: [NSLocalizedDescriptionKey: "Missing required service account fields"])
        }
        
        let signers = JWTSigners()
        try signers.use(.rs256(key: .private(pem: privateKey)))
        
        let now = Date()
        let claims = GoogleClaims(
            iss: clientEmail,
            scope: "https://www.googleapis.com/auth/datastore https://www.googleapis.com/auth/cloud-platform",
            aud: tokenUri,
            exp: .init(value: now.addingTimeInterval(3600)),
            iat: .init(value: now)
        )
        
        let jwt = try signers.sign(claims)
        
        var request = HTTPClientRequest(url: tokenUri)
        request.method = .POST
        request.headers.add(name: "Content-Type", value: "application/x-www-form-urlencoded")
        
        let body = "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=\(jwt)"
        request.body = .bytes(ByteBuffer(string: body))
        
        return try await executeTokenRequest(request)
    }
    
    private func refreshAuthorizedUserToken() async throws -> String {
        logger.debug("Refreshing Google Authorized User Token...")
        
        guard let refreshToken = serviceAccount.refresh_token,
              let clientId = serviceAccount.client_id,
              let clientSecret = serviceAccount.client_secret else {
            throw NSError(domain: "GoogleAuth", code: 400, userInfo: [NSLocalizedDescriptionKey: "Missing required authorized user fields"])
        }
        
        let tokenUri = "https://oauth2.googleapis.com/token"
        var request = HTTPClientRequest(url: tokenUri)
        request.method = .POST
        request.headers.add(name: "Content-Type", value: "application/x-www-form-urlencoded")
        
        let body = "grant_type=refresh_token&client_id=\(clientId)&client_secret=\(clientSecret)&refresh_token=\(refreshToken)"
        request.body = .bytes(ByteBuffer(string: body))
        
        return try await executeTokenRequest(request)
    }
    
    private func executeTokenRequest(_ request: HTTPClientRequest) async throws -> String {
        let response = try await httpClient.execute(request, timeout: .seconds(10))
        
        guard response.status == .ok else {
            let bodyData = try await response.body.collect(upTo: 1024 * 1024)
            let bodyStr = String(buffer: bodyData)
            logger.error("Failed to get token: \(response.status) - \(bodyStr)")
            throw NSError(domain: "GoogleAuth", code: Int(response.status.code), userInfo: [NSLocalizedDescriptionKey: "Failed to get token: \(bodyStr)"])
        }
        
        let bodyData = try await response.body.collect(upTo: 1024 * 1024)
        let tokenResponse = try JSONDecoder().decode(TokenResponse.self, from: Data(buffer: bodyData))
        
        self.currentToken = tokenResponse.access_token
        self.tokenExpiration = Date().addingTimeInterval(TimeInterval(tokenResponse.expires_in - 60)) // Buffer
        
        logger.debug("Token refreshed successfully.")
        return tokenResponse.access_token
    }
    
    nonisolated var projectId: String {
        return serviceAccount.project_id ?? serviceAccount.quota_project_id ?? "unknown"
    }
}