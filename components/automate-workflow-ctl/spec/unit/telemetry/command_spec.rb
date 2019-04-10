require "spec_helper"
require "telemetry/telemetry"
require "telemetry/command"
require "ctl-helpers/exit_handler"

describe Telemetry::Command do
  let(:args) { [] }

  subject(:command) { described_class.new(args) }

  describe "run" do
    before do
      allow(CtlHelpers::ExitHandler).to receive(:handle_exit).and_yield
    end

    it "on a Telemetry::Command object with no arguments it exits with 1" do
      expect(command).to receive(:puts_error)
      expect {command.run}.to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
    end

    context "with 'enable' as an argument" do
      let(:args) { [ "enable" ] }

      it "enables telemetry" do
        expect(Telemetry).to receive(:enable_for_server)
        command.run
      end
    end

    context "with 'disable' as an argument" do
      let(:args) { [ "disable" ] }

      it "disables telemetry" do
        expect(Telemetry).to receive(:disable_for_server)
        command.run
      end
    end

    context "with 'status' as an argument" do
      let(:args) { [ "status" ] }

      it "prints the current telemetry status" do
        expect(Telemetry).not_to receive(:disable_for_server)
        expect(Telemetry).not_to receive(:enable_for_server)
        command.run
      end
    end
  end
end
