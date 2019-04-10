#!/usr/bin/env ruby

require 'omnibus-ctl'

ctl = Omnibus::Ctl.new("delivery")
ctl.load_files(ENV["OMNIBUS_FILES"])
# The omnibus delivery-ctl definition runs the omnibus-ctl executable,
# which seems to append a couple of extra arguments between the command name
# and options. Inserting these 2 spacers bewteen the command name (eg. `create-user`)
# and the rest of the arguments allows the rest of the code to run unmodified
# from the run() method.
arguments = ARGV.insert(1, "", "")
ctl.run(arguments)
exit 0
