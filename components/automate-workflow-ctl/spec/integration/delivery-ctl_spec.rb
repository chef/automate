require 'spec_helper'
require 'omnibus-ctl'
require 'tempfile'

RSpec.configure do |c|
  c.filter_run_excluding :broken => true
end

describe 'automate-ctl' do
  ctl_instance = Omnibus::Ctl.new("automate-ctl")
  ctl_instance.load_files(File.expand_path('../../../lib/', __FILE__))

  # An array of strings of every command in automate-ctl.
  @all_commands = ctl_instance.command_map.keys
  ctl_instance.category_command_map.keys.each do |key|
    @all_commands.concat ctl_instance.category_command_map[key].keys
  end

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
    # OmnibusCtl expects its arguments to be pre-parsed and passed in, but it
    # only makes the command name + next arg available to commands.  To do further option
    # parsing, commands need to look at ARGV directly.
    # The bits below ensure commands see an ARGV that is the same size that they'll
    # get in actual usage within delivery-ctl.
    ARGV.clear
    ARGV << "stub1"
    ARGV << "stub2"
    args.each {|a| ARGV << a }

    # Create a new stream for each command executed to ensure that it contains
    # only the output of the one command.
    ctl.fh_output = StringIO.new

    ctl.run(args)
    ctl.fh_output.rewind
  end

  # These are commands added by default from omnibus-ctl that
  # do not respond to --help. Until we go refactor omnibus-ctl,
  # we need to skip testing these commands.
  # Please do not add your new commands to this list (implement --help instead)!
  @omnibus_ctl_commands = ["show-config", "reconfigure", "cleanse", "int",
                           "help", "uninstall", "service-list"]

  # Note: If one of these tests breaks unexpectedly on you,
  #       it most likely means you wrote a ctl command that
  #       does not properly respond to --help! All automate-ctl
  #       commands should respond to --help so please update your
  #       command to do so. Ping Tyler Cloke if you are still confused.
  @all_commands.each do |command|
    next if @omnibus_ctl_commands.include?(command)
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

    describe 'when the user is not root' do
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
    end
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

    describe "node-summary" do
      before do
        allow(NodeUtils).to receive(:node_summary)
        allow(NodeUtils::Log).to receive(:info)
      end

      context "no parameters" do
        it "outputs node summary as text" do
          expect(NodeUtils)
            .to receive(:node_summary)
            .with(
              format: "text",
              request_timeout: 300
            )

          expect do
            run_ctl_command(["node-summary"])
          end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
        end
      end

      context "--format json --request-timeout 50" do
        it "outputs node summary as json and sets the request timeout" do
          expect(NodeUtils)
            .to receive(:node_summary)
            .with(
              format: "json",
              request_timeout: 50
            )

          expect do
            run_ctl_command(["node-summary", "--format", "json", "--request-timeout", "50"])
          end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
        end
      end
    end

    describe "delete-node" do
      let(:uuid) { "c2a56956-0fe0-4272-9c06-83cb73df022a" }

      before do
        allow(NodeUtils).to receive(:delete_node)
      end

      context "--uuid" do
        it "deletes converge data and skips compliance data" do
          expect(NodeUtils).to receive(:delete_node) do |config|
            expect(config[:node_data]).to eq(true)
            expect(config[:compliance_data]).to eq(false)
            expect(config[:uuid]).to eq(uuid)
            expect(config[:purge]).to eq(false)
            expect(config[:force]).to eq(false)
          end

          expect do
            run_ctl_command(["delete-node", "--uuid", uuid])
          end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
        end
      end

      context "--name --org --chef-server-fqdn" do
        it "deletes converge data and skips compliance data" do
          expect(NodeUtils).to receive(:delete_node) do |config|
            expect(config[:node_data]).to eq(true)
            expect(config[:compliance_data]).to eq(false)
            expect(config[:uuid]).to eq(nil)
            expect(config[:purge]).to eq(false)
            expect(config[:force]).to eq(false)
            expect(config[:name]).to eq("node1")
            expect(config[:org]).to eq("myorg")
            expect(config[:chef_server_fqdn]).to eq("mycompany.net")
          end

          expect do
            run_ctl_command([
              "delete-node", "--name", "node1", "--org", "myorg", "--chef-server-fqdn", "mycompany.net"
            ])
          end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
        end
      end

      context "without a name or uuid" do
        it "raises an error" do
          expect(NodeUtils).to_not receive(:delete_node)
          expect do
            run_ctl_command(["delete-node"])
          end.to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
        end
      end

      context "--force --compliance-data --no-node-data" do
        it "forcibly deletes compliance data and skips node data" do
          expect(NodeUtils).to receive(:delete_node) do |config|
            expect(config[:node_data]).to eq(false)
            expect(config[:compliance_data]).to eq(true)
            expect(config[:uuid]).to eq(uuid)
            expect(config[:purge]).to eq(false)
            expect(config[:force]).to eq(true)
          end

          expect do
            run_ctl_command([
              "delete-node", "-u", uuid, "--force", "--compliance-data", "--no-node-data"
            ])
          end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
        end
      end

      context "--purge" do
        it "purges all data" do
          expect(NodeUtils).to receive(:delete_node) do |config|
            expect(config[:node_data]).to eq(true)
            expect(config[:compliance_data]).to eq(true)
            expect(config[:uuid]).to eq(uuid)
            expect(config[:purge]).to eq(true)
            expect(config[:force]).to eq(false)
          end

          expect do
            run_ctl_command(["delete-node", "-u", uuid, "--purge"])
          end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
        end
      end

      context "with a malformed UUID" do
        it "raises an error" do
          expect(NodeUtils).to_not receive(:delete_node)
          expect do
            run_ctl_command(["delete-node", "-u", "malformed-uuid", "--purge"])
          end.to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
        end

        it "raises an error" do
          expect(NodeUtils).to_not receive(:delete_node)
          expect do
            run_ctl_command(["delete-node", "--uuid", "malformed-uuid", "--purge"])
          end.to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
        end
      end
    end

    describe "data-summary" do
      before do
        allow(NodeUtils).to receive(:data_summary)
        allow(NodeUtils::Log).to receive(:info)
      end

      it "enables all data and gb by default" do
        expect(NodeUtils).to receive(:data_summary) do |config|
          expect(config[:compliance]).to eq(true)
          expect(config[:insights]).to eq(true)
          expect(config[:node_state]).to eq(true)
          expect(config[:cluster]).to eq(true)
          expect(config[:unit_measure]).to eq("gb")
        end

        expect do
          run_ctl_command(["data-summary"])
        end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
      end

      it "-c only displays compliance" do
        expect(NodeUtils).to receive(:data_summary) do |config|
          expect(config[:compliance]).to eq(true)
          expect(config).to_not have_key(:insights)
          expect(config).to_not have_key(:node_state)
          expect(config).to_not have_key(:cluster)
        end

        expect do
          run_ctl_command(["data-summary", "-c"])
        end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
      end

      it "-i only displays insights" do
        expect(NodeUtils).to receive(:data_summary) do |config|
          expect(config[:insights]).to eq(true)
          expect(config).to_not have_key(:compliance)
          expect(config).to_not have_key(:node_state)
          expect(config).to_not have_key(:cluster)
        end

        expect do
          run_ctl_command(["data-summary", "-i"])
        end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
      end

      it "-n only displays node-state" do
        expect(NodeUtils).to receive(:data_summary) do |config|
          expect(config[:node_state]).to eq(true)
          expect(config).to_not have_key(:insights)
          expect(config).to_not have_key(:compliance)
          expect(config).to_not have_key(:cluster)
        end

        expect do
          run_ctl_command(["data-summary", "-n"])
        end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
      end

      it "-s only displays cluster" do
        expect(NodeUtils).to receive(:data_summary) do |config|
          expect(config[:cluster]).to eq(true)
          expect(config).to_not have_key(:insights)
          expect(config).to_not have_key(:compliance)
          expect(config).to_not have_key(:node_state)
        end

        expect do
          run_ctl_command(["data-summary", "-s"])
        end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
      end

      it "-f json sets format to json" do
        expect(NodeUtils).to receive(:data_summary) do |config|
          expect(config[:format]).to eq("json")
        end

        expect do
          run_ctl_command(["data-summary", "-f", "json"])
        end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
      end

      it "-u mb sets unit to mb" do
        expect(NodeUtils).to receive(:data_summary) do |config|
          expect(config[:unit_measure]).to eq("mb")
        end

        expect do
          run_ctl_command(["data-summary", "-u", "mb"])
        end.to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
      end
    end
  end
end
