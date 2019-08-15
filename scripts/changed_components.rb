#!/usr/bin/env ruby

require "json"
require "yaml"
require "tsort"
require "toml"

class Hash
  include TSort
  alias tsort_each_node each_key
  def tsort_each_child(node, &block)
    fetch(node).each(&block)
  end
end

def get_hab_deps(source_dir)
  depString = `bash -c 'cd #{source_dir}; PLAN_CONTEXT=habitat . habitat/plan.sh; echo ${pkg_deps[*]}'`
  depString.lines.last.split
end

config = TOML.load_file(".bldr.toml")

changed_files = `git diff --name-only $(scripts/git_difference_expression.rb)`.split("\n")

build_all = (ENV["BUILDKITE_BRANCH"] || "").include?("verify-rebuild-all") || ENV["BUILD_ALL"] == "true"

#
# The goal here is to produce the same builds that Expeditor would produce
# should the PR be merged. That way we can know whether or not this PR would
# break the pipeline.
#
# We are a bit wasteful here as we could do this in one pass and
# without the O(N^2) searching, but if any of that ends up mattering
# something has gone very very very wrong.
#
# Also only care about the portion of the dep tree we can actually
# build, which means we throw out anything not in this repository. I
# suppose techincally if something outside this repository depended
# on us, we could end up missing a required dependency.
#
changed_packages = Hash.new
changed_package_file_globs = Hash.new
hab_deps = Hash.new

config.each do |pkg_name, metadata|
  # Gather hab dependency information, filtering packages not in this repo
  hab_deps[pkg_name] = get_hab_deps(metadata["plan_path"]).map do |p|
    name = p.split("/")[1]
    name if config[name]
  end.compact

  # Look for changed packages based on file diff
  if build_all
    changed_packages[pkg_name] = metadata["plan_path"]
  else
    globs = Array(metadata["paths"]).select do |glob|
      changed_files.any? { |file| File.fnmatch(glob, file) }
    end
    unless globs.empty?
      changed_packages[pkg_name] = metadata["plan_path"]
      changed_package_file_globs[pkg_name] = globs
    end
  end
end

hab_deps.each do |pkg, deps|
  deps.each do |d|
    changed_packages[pkg] = config[pkg]["plan_path"] if changed_packages[d]
  end
end

hab_deps.tsort.each do |p|
  puts changed_packages[p] if changed_packages[p]
end

changed_package_file_globs.each do |p, globs|
  STDERR.puts "<details><summary>#{p}</summary>\n\n"
  globs.each do |glob|
    STDERR.puts "  - #{glob}"
  end
  STDERR.puts "</details>"
end
