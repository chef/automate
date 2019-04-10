require 'spec_helper'
require 'ctl-helpers/prompt'

describe "yes_no_prompt" do
  shared_examples_for "a yes response" do
    it "returns true" do
      allow(CtlHelpers::Prompt).to receive(:prompt_user).
        with("Do you want to? (yes/no)").
        and_return(response)
      expect(CtlHelpers::Prompt.yes_no_prompt("Do you want to? (yes/no)")).to eq(true)
    end
  end

  %w{y Y yes Yes YES}.each do |resp|
    describe "when the input is '#{resp}'" do
      it_behaves_like "a yes response" do
        let(:response) { resp }
      end
    end
  end

  it "returns false when response is not yes-ey" do
    allow(CtlHelpers::Prompt).to receive(:prompt_user).
        with("Do you want to? (yes/no)").
        and_return("no no no")
      expect(CtlHelpers::Prompt.yes_no_prompt("Do you want to? (yes/no)")).to eq(false)
  end
end

describe "prompt_user" do
  context "when no default is provided" do
    it "reprompts when input is empty" do
      expect(CtlHelpers::Prompt).to receive(:get_user_response).and_return("", "valid")
      CtlHelpers::Prompt.prompt_user("Anything")
    end
    it "reprompts when input is only whitespace" do
      expect(CtlHelpers::Prompt).to receive(:get_user_response).and_return("   ", "\t", "Done")
      CtlHelpers::Prompt.prompt_user("Anything")
    end
  end

  context "when a default is provided" do
    it "uses the provided default when no value is entered" do
      expect(CtlHelpers::Prompt).to receive(:get_user_response).and_return("")
      expect(CtlHelpers::Prompt.prompt_user("Foo", true, "bar")).to eq "bar"
    end
    it "uses the provided default when only whitespace is entered" do
      expect(CtlHelpers::Prompt).to receive(:get_user_response).and_return("   ")
      expect(CtlHelpers::Prompt.prompt_user("Foo", true, "bar")).to eq "bar"
    end
    it "includes the default value in the prompt itself" do
      allow(CtlHelpers::Prompt).to receive(:get_user_response).and_return("")
      expect($stdout).to receive(:puts).with(/[bar]/)
      CtlHelpers::Prompt.prompt_user("Foo", true, "bar")
    end
  end

  it "does not reprompt when input is prefixed with whitespace" do
    expect(CtlHelpers::Prompt).to receive(:get_user_response).and_return " this is valid password"
    CtlHelpers::Prompt.prompt_user("Anything")
  end
  it "trims trailing whitespace" do
    expect(CtlHelpers::Prompt).to receive(:get_user_response).and_return("this should be trimmed  \t  ").exactly(:once)
    expect(CtlHelpers::Prompt.prompt_user("Anything")).to eq("this should be trimmed")
  end
  it "does not trim prefixing whitespace" do
    expect(CtlHelpers::Prompt).to receive(:get_user_response).and_return(" \t this should not be trimmed")
    expect(CtlHelpers::Prompt.prompt_user("Anything")).to eq(" \t this should not be trimmed")
  end

  it "exits with ActionCanceled on interrupt" do
    expect(CtlHelpers::Prompt).to receive(:get_user_response).and_raise(Interrupt.new)
    expect{ CtlHelpers::Prompt.prompt_user("Anything")}.to raise_error(CtlHelpers::Exceptions::ActionCanceled)
  end
end
