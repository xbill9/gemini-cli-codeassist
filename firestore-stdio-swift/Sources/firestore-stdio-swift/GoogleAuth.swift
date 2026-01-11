import AsyncHTTPClient
import Foundation
import JWTKit
import Logging
import NIO
import NIOFoundationCompat
import NIOHTTP1

struct ServiceAccount: Codable {
  let type: String
  let projectId: String?
  let quotaProjectId: String?
  let privateKeyId: String?
  let privateKey: String?
  let clientEmail: String?
  let clientId: String?
  let clientSecret: String?
  let refreshToken: String?
  let authUri: String?
  let tokenUri: String?
  let authProviderX509CertUrl: String?
  let clientX509CertUrl: String?

  enum CodingKeys: String, CodingKey {
    case type
    case projectId = "project_id"
    case quotaProjectId = "quota_project_id"
    case privateKeyId = "private_key_id"
    case privateKey = "private_key"
    case clientEmail = "client_email"
    case clientId = "client_id"
    case clientSecret = "client_secret"
    case refreshToken = "refresh_token"
    case authUri = "auth_uri"
    case tokenUri = "token_uri"
    case authProviderX509CertUrl = "auth_provider_x509_cert_url"
    case clientX509CertUrl = "client_x509_cert_url"
  }
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
  let accessToken: String
  let expiresIn: Int
  let tokenType: String

  enum CodingKeys: String, CodingKey {
    case accessToken = "access_token"
    case expiresIn = "expires_in"
    case tokenType = "token_type"
  }
}

protocol TokenProvider: Sendable {
  func getAccessToken() async throws -> String
  func getProjectId() async throws -> String
}

actor ServiceAccountTokenProvider: TokenProvider {
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
      throw NSError(
        domain: "GoogleAuth", code: 400,
        userInfo: [NSLocalizedDescriptionKey: "Unsupported credential type: \(serviceAccount.type)"]
      )
    }
  }

  private func refreshServiceAccountToken() async throws -> String {
    logger.debug("Refreshing Google Service Account Token...")

    guard let privateKey = serviceAccount.privateKey,
      let clientEmail = serviceAccount.clientEmail,
      let tokenUri = serviceAccount.tokenUri
    else {
      throw NSError(
        domain: "GoogleAuth", code: 400,
        userInfo: [NSLocalizedDescriptionKey: "Missing required service account fields"])
    }

    let signers = JWTSigners()
    try signers.use(.rs256(key: .private(pem: privateKey)))

    let now = Date()
    let claims = GoogleClaims(
      iss: clientEmail,
      scope:
        "https://www.googleapis.com/auth/datastore https://www.googleapis.com/auth/cloud-platform",
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

    guard let refreshToken = serviceAccount.refreshToken,
      let clientId = serviceAccount.clientId,
      let clientSecret = serviceAccount.clientSecret
    else {
      throw NSError(
        domain: "GoogleAuth", code: 400,
        userInfo: [NSLocalizedDescriptionKey: "Missing required authorized user fields"])
    }

    let tokenUri = "https://oauth2.googleapis.com/token"
    var request = HTTPClientRequest(url: tokenUri)
    request.method = .POST
    request.headers.add(name: "Content-Type", value: "application/x-www-form-urlencoded")

    let body =
      "grant_type=refresh_token&client_id=\(clientId)&client_secret=\(clientSecret)&refresh_token=\(refreshToken)"
    request.body = .bytes(ByteBuffer(string: body))

    return try await executeTokenRequest(request)
  }

  private func executeTokenRequest(_ request: HTTPClientRequest) async throws -> String {
    let response = try await httpClient.execute(request, timeout: .seconds(10))

    guard response.status == .ok else {
      let bodyData = try await response.body.collect(upTo: 1024 * 1024)
      let bodyStr = String(buffer: bodyData)
      logger.error("Failed to get token: \(response.status) - \(bodyStr)")
      throw NSError(
        domain: "GoogleAuth", code: Int(response.status.code),
        userInfo: [NSLocalizedDescriptionKey: "Failed to get token: \(bodyStr)"])
    }

    let bodyData = try await response.body.collect(upTo: 1024 * 1024)
    let tokenResponse = try JSONDecoder().decode(TokenResponse.self, from: Data(buffer: bodyData))

    self.currentToken = tokenResponse.accessToken
    /// Buffer 60 seconds
    self.tokenExpiration = Date().addingTimeInterval(TimeInterval(tokenResponse.expiresIn - 60))

    logger.debug("Token refreshed successfully.")
    return tokenResponse.accessToken
  }

  func getProjectId() async throws -> String {
    return serviceAccount.projectId ?? serviceAccount.quotaProjectId ?? "unknown"
  }
}

actor MetadataServerTokenProvider: TokenProvider {
  private let httpClient: HTTPClient
  private let logger: Logger
  private var currentToken: String?
  private var tokenExpiration: Date?
  private var cachedProjectId: String?

  init(httpClient: HTTPClient, logger: Logger) {
    self.httpClient = httpClient
    self.logger = logger
  }

  func getAccessToken() async throws -> String {
    if let token = currentToken, let expiration = tokenExpiration, expiration > Date() {
      return token
    }

    let url =
      "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token"
    var request = HTTPClientRequest(url: url)
    request.headers.add(name: "Metadata-Flavor", value: "Google")

    let response = try await httpClient.execute(request, timeout: .seconds(5))
    guard response.status == .ok else {
      throw NSError(domain: "GoogleAuth", code: Int(response.status.code), userInfo: nil)
    }

    let bodyData = try await response.body.collect(upTo: 1024 * 1024)
    let tokenResponse = try JSONDecoder().decode(TokenResponse.self, from: Data(buffer: bodyData))

    self.currentToken = tokenResponse.accessToken
    self.tokenExpiration = Date().addingTimeInterval(TimeInterval(tokenResponse.expiresIn - 60))
    return tokenResponse.accessToken
  }

  func getProjectId() async throws -> String {
    if let projectId = cachedProjectId {
      return projectId
    }

    let url = "http://metadata.google.internal/computeMetadata/v1/project/project-id"
    var request = HTTPClientRequest(url: url)
    request.headers.add(name: "Metadata-Flavor", value: "Google")

    let response = try await httpClient.execute(request, timeout: .seconds(5))
    guard response.status == .ok else {
      throw NSError(domain: "GoogleAuth", code: Int(response.status.code), userInfo: nil)
    }

    let bodyData = try await response.body.collect(upTo: 1024 * 1024)
    let projectId = String(buffer: bodyData)
    self.cachedProjectId = projectId
    return projectId
  }
}
