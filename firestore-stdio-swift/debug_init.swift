import Foundation

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

let defaultPath = FileManager.default.homeDirectoryForCurrentUser
  .appendingPathComponent(".config/gcloud/application_default_credentials.json").path

print("Checking path: \(defaultPath)")
if FileManager.default.fileExists(atPath: defaultPath) {
    print("File exists.")
    do {
        let data = try Data(contentsOf: URL(fileURLWithPath: defaultPath))
        print("Read \(data.count) bytes.")
        let sa = try JSONDecoder().decode(ServiceAccount.self, from: data)
        print("Decoded successfully. Type: \(sa.type)")
    } catch {
        print("Error: \(error)")
    }
} else {
    print("File does NOT exist.")
}
