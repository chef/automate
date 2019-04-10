module CtlHelpers
  module Prompt
    def self.prompt_user(prompt, echo = true, default = nil)
      final_prompt = make_prompt(prompt, default)
      $stdout.puts final_prompt
      while response = get_user_response(echo)
        response.chomp!
        response.rstrip!
        if response.empty?
          unless default.nil?
            response = default
            break;
          end
        else
          break;
        end
        $stdout.puts final_prompt
      end
      response
    # Exit cleanly on Ctrl+C instead of dumping a stack trace.
    rescue Interrupt
      raise CtlHelpers::Exceptions::ActionCanceled.new
    end

    def self.yes_no_prompt(msg)
      resp = prompt_user(msg).downcase
      %w{y yes}.include?(resp)
    end

    private

    def self.make_prompt(prompt, default = nil)
      if default.nil? || default.empty?
        "#{prompt}: "
      else
        "#{prompt} [#{default}]: "
      end
    end

    # This wrapper around 'gets' simplifies testing by giving us a
    # thing to mock without having to touch $stdin. It will also reprompt
    # on empty value, or spaces.
    def self.get_user_response(echo = true)
      if echo
        $stdin.gets
      else
        $stdin.noecho(&:gets)
      end
    end
  end
end
