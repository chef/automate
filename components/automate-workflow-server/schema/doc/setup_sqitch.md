Setup Mac OS X for Using Sqitch
===============================

We use [sqitch][], a database-agnostic schema change management tool
for dealing with schema changes.  It doesn't require learning another
DSL; you get to use plain old SQL.  Some highlights include:

- dependencies between changesets
- tagging of schema milestones
- deploy or revert to specific schema tags
- detailed deploy and revert event metadata stored in the database
- custom verification scripts to verify the schema at deploy time;
  nothing gets deployed if verification fails (when the database
  supports DDL in transactions, that is, like Postgres)
- support for easy reworking of existing database objects (e.g. stored
  procedures)

[sqitch]:http://sqitch.org

## Installing Sqitch

On OS X, the simplest approach is to use Homebrew:

    brew tap theory/sqitch
    brew install sqitch_pg

(Note that this command is PostgreSQL-specific, which is all we
currently care about.)

## Installing Sqitch Manually

In order to easily install Sqitch manually (and more importantly, to install arbitrary versions of Sqitch), I recommend using [Perlbrew](http://perlbrew.pl) (think RVM for Perl). There are issues installing Sqitch into the Perl that ships with OS X (Yosemite, at least), so having your own Perl "sandbox" just makes things a lot easier.

    sudo cpan App::perlbrew
    perlbrew init

Add the following to your shell startup file (`~/.bashrc`, `~/.zshrc`, etc.):

    source ~/perl5/perlbrew/etc/bashrc

Install a Perl distribution:

    perlbrew install perl-5.18.4
    perlbrew switch perl-5.18.4
    perlbrew install-cpanm

To use this Perl permanently, add the following to your shell startup file:

    perlbrew switch perl-5.18.4

Finally, install Sqitch (this happens to be the version currently shipping with Delivery):

    cpanm --notest DBD::Pg
    cpanm --notest App::Sqitch@0.973
