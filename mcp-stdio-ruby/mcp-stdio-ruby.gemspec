# frozen_string_literal: true

Gem::Specification.new do |spec|
  spec.name          = 'mcp-stdio-ruby'
  spec.version       = '0.1.0'
  spec.authors       = ['xbill']
  spec.email         = ['xbill9@gmail.com']

  spec.summary       = 'A simple Model Context Protocol (MCP) server implemented in Ruby.'
  spec.description   = 'Exposes tools over HTTP (SSE) using the MCP SDK.'
  spec.homepage      = 'https://github.com/xbill9/gemini-cli-codeassist/mcp-stdio-ruby'
  spec.license       = 'MIT'
  spec.required_ruby_version = '>= 3.0.0'

  spec.metadata['homepage_uri'] = spec.homepage
  spec.metadata['source_code_uri'] = 'https://github.com/xbill9/gemini-cli-codeassist/mcp-stdio-ruby'
  spec.metadata['changelog_uri'] = 'https://github.com/xbill9/gemini-cli-codeassist/mcp-stdio-ruby/blob/main/CHANGELOG.md'
  spec.metadata['rubygems_mfa_required'] = 'true'

  # Specify which files should be added to the gem when it is released.
  spec.files = Dir.chdir(File.expand_path(__dir__)) do
    Dir['{lib}/**/*', 'GEMINI.md', 'README.md', 'LICENSE.txt']
  end
  spec.bindir        = 'bin'
  spec.executables   = ['mcp-https-ruby']
  spec.require_paths = ['lib']

  spec.add_dependency 'logger'
  spec.add_dependency 'mcp'
  spec.add_dependency 'puma'
  spec.add_dependency 'rack'
  spec.add_dependency 'rackup'
end
