require 'spec_helper.rb'

require 'runner/install/options'
require 'runner/exceptions'

describe Runner::Install::Options do
  let(:desc) { "desc" }

  it "raises an error when it receives a command line flag it doesn't know about" do
    expect { described_class.new(["--hello", "world"], desc) }
      .to raise_error(OptionParser::InvalidOption)
  end

  shared_examples_for "returns the passed argument" do
    it "returns the provided argument when --flag is given" do
      option_parser = described_class.new(["--#{flag}", "somevalue"], desc)
      expect(option_parser.options[name.to_sym]).to eq("somevalue")
    end
  end

  context "when flags that take manditory arguments are passed" do
    [:installer, :"chefdk-version", :port,
     "ssh-identity-file", "fips-custom-cert-filename"].each do |target|
      let(:name) { target.to_s.gsub("-", "_") }
      let(:flag) { target }

      it_behaves_like "returns the passed argument"

      it "raises an error when --#{target} is provided with no value" do
        expect { described_class.new(["--#{target}"], desc) }
          .to raise_error(OptionParser::MissingArgument)
      end
    end
  end

  context "when a flag takes an optional value" do
    let(:flag) { :password }
    let(:name) { :password }

    it_behaves_like "returns the passed argument"
  end
end
