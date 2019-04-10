require 'spec_helper.rb'

require 'ctl-helpers/color_printer'

class DummyClass
end

describe CtlHelpers::ColorPrinter do
  let(:color_printer) { DummyClass.new }

  before do
    color_printer.extend(CtlHelpers::ColorPrinter)
  end

  shared_examples "a print method" do
    let(:colorize_string_response) { "colorized string" }

    before do
      expect(color_printer).to receive(:colorize_string).with(expected_string, color).and_return(colorize_string_response)
      expect(color_printer).to receive(output_method).with(colorize_string_response)
    end

    context "no arguments are passed" do
      let(:color) { :default }
      let(:expected_string) { "" }
      it "calls puts_color_to_stream with the default color and an empty string" do
        color_printer.send(method_under_test)
      end
    end

    context "when a string is passed" do
      let(:color) { :default }
      let(:expected_string) { "some_string" }

      it "calls puts_color_to_stream with the default color and passed string" do
        color_printer.send(method_under_test, expected_string)
      end
    end

    context "when a string and a color are passed" do
      let(:color) { :red }
      let(:expected_string) { "some_string" }

      it "calls puts_color_to_stream with the passed color and string" do
        color_printer.send(method_under_test, expected_string, color)
      end
    end
  end

  describe "#puts" do
    let(:method_under_test) { :puts }
    let(:output_method) { :puts_color_to_stream }

    it_behaves_like "a print method"
  end

  describe "#print" do
    let(:method_under_test) { :print }
    let(:output_method) { :print_color_to_stream }

    it_behaves_like "a print method"
  end

  describe "#puts_*" do
    let(:expected_string) { "some_string" }

    before do
      allow(String).to receive(:colors).and_return([:red, :white, :blue])
    end

    context "when calling #puts_* for a color that exists" do
      [:red, :white, :blue].each do |color|
        describe "#puts_#{color}" do
          it "calls puts with the proper color" do
            expect(color_printer).to receive(:puts).with(expected_string, color)
            color_printer.send("puts_#{color.to_s}", expected_string)
          end
        end
      end
    end

    context "when calling #puts_* for a color that does not exist" do
      it "throws a NoMethodError" do
        expect{ color_printer.puts_not_a_color(expected_string) }
          .to raise_error NoMethodError
      end
    end
  end

  describe "#puts_error" do
    let(:error_str) { "some error message" }

    context "when no extra info is supplied" do
      it "prints the error in red with the leading ERROR: and no extra error" do
        expect(color_printer)
          .to receive(:print).with("ERROR: ", CtlHelpers::ColorPrinter::ERROR_WARNING_COLOR)
        expect(color_printer).to receive(:puts).once.with("some error message\n")
        color_printer.puts_error(error_str)
      end
    end

    context "when extra info is supplied" do
      let(:extra_error_str) { "extra error message" }

      it "prints the error in red with the leading ERROR: and no extra error" do
        expect(color_printer)
          .to receive(:print).with("ERROR: ", CtlHelpers::ColorPrinter::ERROR_WARNING_COLOR)
        expect(color_printer).to receive(:puts).with("some error message\n")
        expect(color_printer).to receive(:puts).with(extra_error_str)
        color_printer.puts_error(error_str, extra_error_str)
      end
    end
  end

  describe "#puts_information" do
    let(:string) { "output string" }

    it "puts with the information color" do
      expect(color_printer).to receive(:puts).with(string, CtlHelpers::ColorPrinter::INFORMATION_COLOR)
      color_printer.puts_information(string)
    end
  end

  describe "#print_information" do
    let(:string) { "output string" }

    it "prints with the information color" do
      expect(color_printer).to receive(:print).with(string, CtlHelpers::ColorPrinter::INFORMATION_COLOR)
      color_printer.print_information(string)
    end
  end

  describe "#puts_warning" do
    let(:string) { "output string" }

    it "puts with the warning color" do
      expect(color_printer).to receive(:puts).with(string, CtlHelpers::ColorPrinter::ERROR_WARNING_COLOR)
      color_printer.puts_warning(string)
    end
  end

  describe "#print_warning" do
    let(:string) { "output string" }

    it "prints with the warning color" do
      expect(color_printer).to receive(:print).with(string, CtlHelpers::ColorPrinter::ERROR_WARNING_COLOR)
      color_printer.print_warning(string)
    end
  end

  describe "#puts_top_level_step" do
    let(:string) { "output string" }

    it "calls puts_information with an ellipsis added" do
      expect(color_printer).to receive(:puts_information).with(string + "...")
      color_printer.puts_top_level_step(string)
    end
  end

  describe "#puts_substep" do
    let(:string) { "output string" }

    it "calls puts_default with relevant tabbing" do
      expect(color_printer).to receive(:puts_default).with("  " + string)
      color_printer.puts_substep(string)
    end
  end
end
