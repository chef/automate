#!/usr/bin/env ruby

require 'yaml'

steps = []
ARGV.each do |f|
  pipeline = YAML.load_file(f)
  pipeline_name = File.basename(f).split(".").first
  expeditor_defaults = pipeline.dig("expeditor", "defaults") || {}
  pipeline["steps"].each do |step|
    next if step == "wait"
    s = Hash.new("")
    s["pipeline"] = pipeline_name
    s["label"] = step["label"]
    if e = step.dig("expeditor", "executor", "linux")
      s["executor"] = "linux"
    elsif e = step.dig("expeditor", "executor", "docker")
      s["executor"] = "docker"
    elsif e = expeditor_defaults.dig("executor", "linux")
      s["executor"] = "linux"
    elsif e = expeditor_defaults.dig("executor", "docker")
      s["executor"] = "docker"
    end

    if e
      s["usage"] = "single-use" if e["single-use"]
      s["privileged"] = "privileged" if e["privileged"]
    end
    steps << s
  end
end

longest_label = 0
steps.each do |s|
  if s["label"].length > longest_label
       longest_label = s["label"].length
  end
end

fmtStr = "%#{longest_label}s %6s %10s %10s %s\n"
steps.each do |s|
  printf fmtStr, s["label"], s["executor"], s["usage"], s["privileged"], s["pipeline"]
end
