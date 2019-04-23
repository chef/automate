require 'spec_helper'
require 'automate_ctl/ctl'
require 'tempfile'

RSpec.configure do |c|
  c.filter_run_excluding :broken => true
end

describe 'workflow-ctl' do
  ctl_instance = AutomateCtl::Ctl.new()
  ctl_instance.load_files(File.expand_path('../../../lib/', __FILE__))

  # An array of strings of every command in workflow-ctl.
  @all_commands = ctl_instance.command_map.keys
  let(:ctl) { ctl_instance }
  let(:file) { double('customer_id file')}
  let(:delivery_running_json) do
    JSON.parse(File.read(File.expand_path((File.join(__dir__, "../fixtures/delivery-running.json")))))
  end

  before do
    allow(FileUtils).to receive(:mkdir_p).with('/var/opt/delivery')
    allow(FileUtils).to receive(:touch).with('/var/opt/delivery/.license.accepted')
  end

  def run_ctl_command(args)
    # Ctl expects its arguments to be pre-parsed and passed in, but it
    # only makes the command name + next arg available to commands.  To do further option
    # parsing, commands need to look at ARGV directly.
    # The bits below ensure commands see an ARGV that is the same size that they'll
    # get in actual usage within workflow-ctl.
    ARGV.clear
    args.each {|a| ARGV << a }

    # Create a new stream for each command executed to ensure that it contains
    # only the output of the one command.
    ctl.fh_output = StringIO.new

    ctl.run(args)
    ctl.fh_output.rewind
  end

  # Note: If one of these tests breaks unexpectedly on you,
  #       it most likely means you wrote a ctl command that
  #       does not properly respond to --help! All workflow-ctl
  #       commands should respond to --help so please update your
  #       command to do so. Ping Tyler Cloke if you are still confused.
  @all_commands.each do |command|
    context "when the user doesn't know how to run #{command}" do
      # Some of the backup commands need to mock out loading the config
      # before options and therefore --help can be parsed.
      before do
        allow($stdout).to receive(:puts)
        allow($stdout).to receive(:print)
      end

      it 'all commands should quickly respond to --help and exit zero' do
        begin
          Timeout::timeout(10) do
            run_ctl_command(%W{#{command} --help})
          end
        rescue SystemExit => e
          expect(e.status).to eq(0)
        end
      end
    end

    describe "when the user runs #{command} and is not root" do
      before do
        allow(Process).to receive(:uid).and_return(1)
      end

      it 'exits 1' do
        begin
          run_ctl_command([command])
        rescue SystemExit => e
          expect(e.status).to eq(1)
        end
      end
    end unless command == "help"
  end

  describe 'when user is not root' do
    before do
      allow(Process).to receive(:uid).and_return(1)
    end

    describe 'help' do
      let(:command) { 'help' }

        it "exits 0" do
          expect {
            run_ctl_command([command])
          }.to raise_error { |error|
            expect(error.status).to eq(0)
          }
        end
      end

    describe '--help' do
      let(:command) { '--help' }

        it "exits 0" do
          expect {
            run_ctl_command([command])
          }.to raise_error { |error|
            expect(error.status).to eq(0)
          }
        end
      end
    end

  describe 'when user is root' do
    before do
      allow(Process).to receive(:uid).and_return(0)
    end

    describe 'help' do
      let(:command) { 'help' }

        it "exits 0" do
          expect {
            run_ctl_command([command])
          }.to raise_error { |error|
            expect(error.status).to eq(0)
          }
        end
      end

    describe '--help' do
      let(:command) { '--help' }

      it "exits 0" do
        expect {
          run_ctl_command([command])
        }.to raise_error { |error|
          expect(error.status).to eq(0)
        }
      end
    end
  end
end
