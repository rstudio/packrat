module Packrat
  puts 'loaded'
  class RCode < Liquid::Block
    def initialize(tag_name, markup, tokens)
      super
    end

    def render(context)
      code = super
      code.gsub!(/^    /m, '') if code =~ /^    /
      code = CGI.escapeHTML code
      code.gsub!(/^((&gt;|\$) [^\r\n]*)$/m, '<span class="command">\\1</span>')
      "<pre>#{code}</pre>"
    end
  end
end

Liquid::Template.register_tag('rcode', Packrat::RCode)
