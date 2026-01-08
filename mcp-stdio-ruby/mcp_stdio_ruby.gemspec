# frozen_string_literal: true

Gem::Specification.new do |spec|
  spec.name          = 'mcp_stdio_ruby'
  spec.version       = '1.0.2'
  spec.authors       = ['xbill']
  spec.email         = ['xbill@glitnir.com']

  spec.summary       = 'A Ruby based Model Context Protocol (MCP) server.'
  spec.description   = 'A Ruby implementation of a Model Context Protocol (MCP) server using stdio transport.'
  spec.homepage      = 'https://github.com/xbill9/gemini-cli-codeassist'
  spec.license       = 'MIT'
  spec.required_ruby_version = '>= 3.0.0'

  spec.metadata['homepage_uri'] = spec.homepage
  spec.metadata['source_code_uri'] = "https://github.com/xbill9/gemini-cli-codeassist/mcp_stdio_ruby"
  spec.metadata['changelog_uri'] = "https://github.com/xbill9/gemini-cli-codeassist/mcp_stdio_ruby/blob/main/CHANGELOG.md"

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files = Dir.chdir(File.expand_path(__dir__)) do
    `git ls-files -z`.split("\x0").reject { |f| f.match(%r{\A(?:test|spec|features)/}) }
  end
  spec.bindir        = 'exe'
  spec.executables   = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  spec.require_paths = ['lib']

  spec.add_dependency 'mcp', '~> 0.1'
  spec.add_dependency 'logger', '~> 1.6'

  spec.add_development_dependency 'rake', '~> 13.0'
  spec.add_development_dependency 'rspec', '~> 3.0'
  spec.add_development_dependency 'rubocop', '~> 1.0'
  spec.add_development_dependency 'rubocop-rspec', '~> 3.0'
end
