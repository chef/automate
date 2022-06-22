Assert.configure do |config|
  config.view.styled true
  config.view.pass_styles :bold, :green
  config.view.error_styles :bold, :red
  config.view.pass_abbrev ' ✔ '
  config.view.fail_abbrev ' ✘ '
  config.view.skip_abbrev ' ↺ '
  config.view.error_abbrev '❕'
end
