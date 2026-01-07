# frozen_string_literal: true

Gem::Specification.new do |spec|
  spec.name          = 'firestore-https-ruby'
  spec.version       = '0.1.0'
  spec.authors       = ['xbill']
  spec.email         = ['xbill9@gmail.com']

  spec.summary       = 'A Model Context Protocol (MCP) server for Google Cloud Firestore.'
  spec.description   = 'Exposes Firestore operations (inventory management) as MCP tools over stdio.'
  spec.homepage      = 'https://github.com/xbill9/gemini-cli-codeassist/firestore-https-ruby'
  spec.license = 'MIT'
  spec.required_ruby_version = '>= 3.1.0'

  spec.metadata['homepage_uri'] = spec.homepage
  spec.metadata['source_code_uri'] = 'https://github.com/xbill9/gemini-cli-codeassist/firestore-https-ruby'
  spec.metadata['bug_tracker_uri'] = 'https://github.com/xbill9/gemini-cli-codeassist/firestore-https-ruby/issues'
  spec.metadata['rubygems_mfa_required'] = 'true'
  # Specify which files should be added to the gem when it is released.
  spec.files = Dir.chdir(File.expand_path(__dir__)) do
    Dir['{lib}/**/*', 'main.rb', 'README.md', 'GEMINI.md', 'LICENSE']
  end

  spec.bindir        = 'bin'
  spec.executables   = spec.files.grep(%r{\Abin/}) { |f| File.basename(f) }
  spec.require_paths = ['lib']

  spec.add_dependency 'dotenv', '~> 3.2'
  spec.add_dependency 'logger', '~> 1.7'
end
