require 'spec_helper.rb'

require 'ctl-helpers/basic_usage'

describe CtlHelpers::BasicUsage do
  let(:usage_line) { "my-delivery-ctl-subcommand ARGS [options...]" }
  let(:description) { "this is my description" }
  let(:basic_usage) { described_class.new(args, usage_line, description) }

  shared_examples_for "--help not passed" do
    it "does nothing" do
      expect(basic_usage).to_not receive(:puts_usage)
      basic_usage.parse_help_and_display_usage
    end
  end

  shared_examples_for "--help passed" do
    it "outputs properly and exits zero" do
      expect(basic_usage).to receive(:puts_usage).with(usage_line, description)
      begin
        basic_usage.parse_help_and_display_usage
        raise
      rescue SystemExit => e
        expect(e.status).to eq(0)
      end
    end
  end

  describe "#parse_help_and_display_usage" do
    context "when no args are passed" do
      let(:args) { [] }

      it_should_behave_like "--help not passed"
    end

    context "when options and args are passed that are not help" do
      let(:args) { ["arg1", "--not-help"] }

      it_should_behave_like "--help not passed"
    end

    context "when --help is passed" do
      let(:args) { ["--help"] }

      it_should_behave_like "--help passed"
    end

    context "when --help is passed with other args" do
      let(:args) { ["args", "but", "--help", "was", "passed"] }

      it_should_behave_like "--help passed"
    end
  end
end
