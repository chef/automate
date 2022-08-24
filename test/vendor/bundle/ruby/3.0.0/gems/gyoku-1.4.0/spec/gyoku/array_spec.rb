require "spec_helper"

describe Gyoku::Array do

  describe ".to_xml" do
    it "returns the XML for an Array of Hashes" do
      array = [{ :name => "adam" }, { :name => "eve" }]
      result = "<user><name>adam</name></user><user><name>eve</name></user>"

      expect(to_xml(array, "user")).to eq(result)
    end

    it "returns the XML for an Array of Hashes unwrapped" do
      array = [{ :name => "adam" }, { :name => "eve" }]
      result = "<user><name>adam</name><name>eve</name></user>"

      expect(to_xml(array, "user", true, {}, :unwrap => true)).to eq(result)
    end

    it "returns the XML for an Array of different Objects" do
      array = [:symbol, "string", 123]
      result = "<value>symbol</value><value>string</value><value>123</value>"

      expect(to_xml(array, "value")).to eq(result)
    end

    it "defaults to escape special characters" do
      array = ["<tag />", "adam & eve"]
      result = "<value>&lt;tag /&gt;</value><value>adam &amp; eve</value>"

      expect(to_xml(array, "value")).to eq(result)
    end

    it "does not escape special characters when told to" do
      array = ["<tag />", "adam & eve"]
      result = "<value><tag /></value><value>adam & eve</value>"

      expect(to_xml(array, "value", false)).to eq(result)
    end

    it "adds attributes to a given tag" do
      array = ["adam", "eve"]
      result = '<value active="true">adam</value><value active="true">eve</value>'

      expect(to_xml(array, "value", :escape_xml, :active => true)).to eq(result)
    end

    it "adds attributes to tags when :unwrap is true" do
      array = [{:item=>"abc"}]
      key = "items"
      escape_xml = :escape_xml
      attributes = { "amount"=>"1" }
      options = { :unwrap => true }
      result = "<items amount=\"1\"><item>abc</item></items>"

      expect(to_xml(array, key, escape_xml, attributes, options)).to eq result
    end

    it "adds attributes to duplicate tags" do
      array = ["adam", "eve"]
      result = '<value id="1">adam</value><value id="2">eve</value>'

      expect(to_xml(array, "value", :escape_xml, :id => [1, 2])).to eq(result)
    end

    it "skips attribute for element without attributes if there are fewer attributes than elements" do
      array = ["adam", "eve", "serpent"]
      result = '<value id="1">adam</value><value id="2">eve</value><value>serpent</value>'

      expect(to_xml(array, "value", :escape_xml, :id => [1, 2])).to eq(result)
    end

    it "handles nested Arrays" do
      array = [["one", "two"]]
      result = "<value><element>one</element><element>two</element></value>"

      expect(to_xml(array, "value")).to eq(result)
    end

    context "when :pretty_print option is set to true" do
      context "when :unwrap option is set to true" do
        it "returns prettified xml" do
          array = ["one", "two", {"three" => "four"}]
          options = { pretty_print: true, unwrap: true }
          result = "<test>\n  <test>one</test>\n  <test>two</test>\n  <three>four</three>\n</test>"
          expect(to_xml(array, "test", true, {}, options)).to eq(result)
        end

        context "when :indent option is specified" do
          it "returns prettified xml with specified indent" do
            array = ["one", "two", {"three" => "four"}]
            options = { pretty_print: true, indent: 3, unwrap: true }
            result = "<test>\n   <test>one</test>\n   <test>two</test>\n   <three>four</three>\n</test>"
            expect(to_xml(array, "test", true, {}, options)).to eq(result)
          end
        end

        context "when :compact option is specified" do
          it "returns prettified xml with specified compact mode" do
            array = ["one", {"two" => "three"}]
            options = { pretty_print: true, compact: false, unwrap: true }
            result = "<test>\n  <test>\n    one\n  </test>\n  <two>\n     three \n  </two>\n</test>"
            expect(to_xml(array, "test", true, {}, options)).to eq(result)
          end
        end
      end

      context "when :unwrap option is not set" do
        it "returns non-prettified xml" do
          array = ["one", "two", {"three" => "four"}]
          options = { pretty_print: true }
          result = "<test>one</test><test>two</test><test><three>four</three></test>"
          expect(to_xml(array, "test", true, {}, options)).to eq(result)
        end
      end
    end
  end

  def to_xml(*args)
    Gyoku::Array.to_xml *args
  end

end
