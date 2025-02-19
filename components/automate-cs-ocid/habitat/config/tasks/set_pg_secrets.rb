require 'json'

class PGDatabaseConfig

  def pg_superuser_id_from_env
    fd = ENV['CHEF_SECRETS_FD']
    if fd
      f = IO.for_fd(fd.to_i)
      secrets = JSON.parse(f.read())
      secrets['userconfig']['pg_superuser_password']
    else
      STDERR.puts "No PG secrets data found in environment"
      raise "No PG secrets data found in environment"
    end
  end

  def get_pg_database_uri()
    dbURL = ENV['DATABASE_URL']
    if dbURL.match('redacted') != nil
      userpass = pg_superuser_id_from_env
      puts dbURL.dup.sub!('<redacted>',userpass)
    else
      puts dbURL
      dbURL
    end
  end

end

PGDatabaseConfig.new.get_pg_database_uri
