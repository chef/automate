require 'spec_helper'
require 'cluster/logger'

describe AutomateCluster::Logger do
  let(:logger) { AutomateCluster::Logger.new }
  let(:testio) { StringIO.new }
  before do
    logger.handlers = [[:console, output: testio]]
  end

  it 'should have a log level of info by default' do
    expect(logger.level).to eq :info
  end

  it 'should have a set the log level' do
    logger.level = :debug
    expect(logger.level).to eq :debug
  end

  it 'should log info by default' do
    logger.info 'info output'
    logger.debug 'debug output'
    expect(testio.string).to match(/info output/)
    expect(testio.string).not_to match(/debug output/)
  end

  it 'should log debug message when log level is set' do
    logger.level = :debug
    logger.info 'info output'
    logger.debug 'debug output'
    expect(testio.string).to match(/info output/)
    expect(testio.string).to match(/debug output/)
  end

  it 'should filter out secrets' do
    logger.filter(%w{ secretpassword supersecret })
    logger.info 'this should not have a secretpassword or supersecret phrase'
    expect(testio.string).to match(/this should not have a \[FILTERED\] or \[FILTERED\] phrase/)
  end
end
