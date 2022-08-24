require "spec_helper"

describe Gyoku::Prettifier do
  describe "#prettify" do
    context "when xml is valid" do
      let!(:xml) { Gyoku::Hash.build_xml(test: { pretty: "xml" }) }

      it "returns prettified xml" do
        expect(subject.prettify(xml)).to eql("<test>\n  <pretty>xml</pretty>\n</test>")
      end

      context "when indent option is specified" do
        it "returns prettified xml with indent" do
          options = { indent: 3 }
          subject = Gyoku::Prettifier.new(options)
          expect(subject.prettify(xml)).to eql("<test>\n   <pretty>xml</pretty>\n</test>")
        end
      end

      context "when compact option is specified" do
        it "returns prettified xml with indent" do
          options = { compact: false }
          subject = Gyoku::Prettifier.new(options)
          expect(subject.prettify(xml)).to eql("<test>\n  <pretty>\n    xml\n  </pretty>\n</test>")
        end
      end
    end

    context "when xml is not valid" do
      let!(:xml) do
        Gyoku::Array.build_xml(["one", "two"], "test")
      end

      it "raises an error" do
        expect{ subject.prettify(xml) }.to raise_error REXML::ParseException
      end
    end
  end
end
