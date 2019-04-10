require 'spec_helper.rb'

require 'runner/chefdk_package'
require 'runner/exceptions'

describe Runner::ChefDKPackage do
  let(:os_name) { "ubuntu" }
  let(:os_release) { "12.04\r" }
  let(:version) { nil }
  let(:chefdk_package) { described_class.new(version, os_name, os_release) }

  describe "#download" do
    context "when the version passed was nil" do
      shared_examples "downloads the ChefDK and puts it in tmp" do
        let(:artifact) { instance_double("artifact") }
        let(:options) { instance_double("options") }
        let(:downloaded_file) { instance_double("file") }
        let(:downloaded_file_content) { "file content" }
        let(:url) { "https://packages.chef.io/stable/ubuntu/12.04/chefdk_#{version}-1_amd64.deb" }
        let(:package_name) { "chefdk_#{version}-1_amd64.deb" }
        let(:filepath) { "/tmp/#{package_name}" }

        before do
          expect(chefdk_package).to receive(:fetch_artifact_package_info)
                                     .with(options).and_return(artifact)
          expect(chefdk_package).to receive(:populate_artifact_options)
                                     .with(download_version_flag).and_return(options)
          expect(chefdk_package).to receive(:open)
                                     .with(url).and_return(downloaded_file)

          expect(artifact).to receive(:url).twice.and_return(url)

          expect(File).to receive(:open).with(filepath, 'w+').and_call_original

          expect(downloaded_file).to receive(:read).and_return(downloaded_file_content)
        end


        after do
          File.delete(filepath)
        end

        it "downloads the chefdk package using mixlib-install to generate the URL" do
          expect(chefdk_package.download).to eq(filepath)
          expect(File.read(filepath)).to eq(downloaded_file_content)
        end
      end

      context "when --chefdk-version was specified" do
        let(:version) { "0.16.12" }
        let(:download_version_flag) { version }

        it_should_behave_like "downloads the ChefDK and puts it in tmp"
      end

      context "when --chefdk-version was not specified" do
        let(:version) { nil }
        let(:download_version_flag) { :latest }

        it_should_behave_like "downloads the ChefDK and puts it in tmp"
      end
    end
  end

  describe '#find_os_info' do
    context "when the os is ubuntu" do
      let(:os_name) { 'ubuntu' }
      let(:os_release) { "12.04\r" }

      it "returns an array of os name and the stripped version" do
        expect(chefdk_package.find_os_info).to eq({:name => "ubuntu", :release => "12.04"})
      end
    end

    context "when the os is debian" do
      let(:os_name) { 'debian' }
      let(:os_release) { "8.6\r" }

      it "returns an array of os name and the stripped version" do
        expect(chefdk_package.find_os_info).to eq({:name => "debian", :release => "8"})
      end
    end

    shared_examples_for "download EL package" do
      it "returns an array of el and the major version" do
        expect(chefdk_package.find_os_info).to eq({:name => "el", :release => "6"})
      end
    end

    context "when the os is suse" do
      let(:os_name) { 'suse' }
      let(:os_release) { "11" }

      it "returns an array of os name and the stripped version" do
        expect(chefdk_package.find_os_info).to eq({:name => "sles", :release => "11"})
      end
    end

    context "when the os is centos" do
      let(:os_name) { 'centos' }
      let(:os_release) { '6.123.4134' }

      it_should_behave_like "download EL package"
    end

    context "when the os is redhat" do
      let(:os_name) { 'redhat' }
      let(:os_release) { '6.123.4134' }

      it_should_behave_like "download EL package"
    end

    context "when the os is oracle" do
      let(:os_name) { 'oracle' }
      let(:os_release) { '6.7' }

      it_should_behave_like "download EL package"
    end

    context "when the os is not supported" do
      let(:os_name) { 'poke-os' }

      it "raises an Runner::Exceptions::UnsupportedTargetOS" do
        expect{ chefdk_package.find_os_info }.to raise_error(Runner::Exceptions::UnsupportedTargetOS)
      end
    end
  end

  describe '#fetch_artifact_package_info' do
    let(:options) { {} }
    let(:artifact) { instance_double("artifact") }

    before do
      expect(Mixlib::Install).to receive(:new).with(options).and_return(artifact)
      expect(artifact).to receive(:artifact_info).and_return(artifact_info)
    end

    context "when the artifact exists" do
      let(:artifact_info) { instance_double(Mixlib::Install::ArtifactInfo) }

      before do
        expect(artifact_info).to receive(:is_a?)
                                  .with(Mixlib::Install::ArtifactInfo).and_return(true)
      end

      it "returns the mixlib install artifact" do
        expect(chefdk_package.fetch_artifact_package_info(options)).to eq(artifact_info)
      end
    end

    context "when the artifact doesn't exist" do
      let(:artifact_info) { [] }

      context "when the user passed a desired chefdk version" do
        let(:version) { "some_invalid_version_string" }

        it "raises a Runner::Exceptions::ChefDK404" do
          expect{ chefdk_package.fetch_artifact_package_info(options) }
            .to raise_error(Runner::Exceptions::ChefDK404)
        end
      end

      context "when the user did not specify a version" do
        let(:version) { nil }

        it "raises a Runner::Exceptions::ChefDKHTTPError" do
          expect{ chefdk_package.fetch_artifact_package_info(options) }
            .to raise_error(Runner::Exceptions::ChefDKHTTPError)
        end
      end
    end
  end
end
