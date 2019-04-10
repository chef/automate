require 'ctl-helpers/prompt'

module Setup
  class InstallRunnerHelper

    DEFAULT_PROMPTS = {
      "Please enter the fully qualified domain name of the remote host that will be configured into a runner" => [true, nil, nil],

      "Please enter the username used for authentication to the remote host that will be configured into a runner" => [true, nil, nil],

      "Please enter the password if you need to set a password for ssh and / or sudo access. (If an SSH identity-file is also input, will only be used for sudo access.) Optional: if not adding a password, please hit enter" => [false, "-P", ""],

      "Please enter the location of the ChefDK package local to this server to install on the runner. Optional: If not passed, the latest ChefDK will be downloaded remotely. Optional: if not adding a location, please hit enter" => [true, "-I", ""],

      "Please enter the SSH identity file used for authentication. Optional: if using a password for SSH authentication, please hit enter" => [true, "-i", ""],

      "Please enter the port to connect to on the remote host. Optional: if using default SSH port (22), please hit enter" => [true, "-p", ""],
    }

    def initialize(prompts = nil)
      @prompts = prompts || DEFAULT_PROMPTS
      @options_array = []
    end

    def gather_options
      @prompts.each do |prompt, prompt_options|
        user_specified_arg = CtlHelpers::Prompt.prompt_user(prompt,
                                                            prompt_options[0],
                                                            prompt_options[2])
        if user_specified_arg != ""
          # if the argument is an option, push the flag into the array before
          # we append the user specified arg.
          if prompt_options[1]
            @options_array.push(prompt_options[1])
          end
          @options_array.push(user_specified_arg)
        end

      end
      @options_array
    end

  end
end
