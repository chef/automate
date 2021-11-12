# We have to monkey patch the Net::HTTP:connect method in order to disable SNI because it does not
# work with the Habitat Supervisor HTTP Gateway.
#
# This is accomplished by commenting out this line:
# s.hostname = @address if s.respond_to? :hostname=
# https://bugs.ruby-lang.org/attachments/4940
#
module Net
  class HTTP
    alias_method :old_connect, :connect

    def connect
      if proxy? then
        conn_address = proxy_address
        conn_port    = proxy_port
      else
        conn_address = address
        conn_port    = port
      end

      D "opening connection to #{conn_address}:#{conn_port}..."
      s = Timeout.timeout(@open_timeout, Net::OpenTimeout) {
        begin
          TCPSocket.open(conn_address, conn_port, @local_host, @local_port)
        rescue => e
          raise e, "Failed to open TCP connection to " +
            "#{conn_address}:#{conn_port} (#{e.message})"
        end
      }
      s.setsockopt(Socket::IPPROTO_TCP, Socket::TCP_NODELAY, 1)
      D "opened"
      if use_ssl?
        if proxy?
          plain_sock = BufferedIO.new(s, read_timeout: @read_timeout,
                                      continue_timeout: @continue_timeout,
                                      debug_output: @debug_output)
          buf = "CONNECT #{@address}:#{@port} HTTP/#{HTTPVersion}\r\n"
          buf << "Host: #{@address}:#{@port}\r\n"
          if proxy_user
            credential = ["#{proxy_user}:#{proxy_pass}"].pack('m0')
            buf << "Proxy-Authorization: Basic #{credential}\r\n"
          end
          buf << "\r\n"
          plain_sock.write(buf)
          HTTPResponse.read_new(plain_sock).value
          # assuming nothing left in buffers after successful CONNECT response
        end

        ssl_parameters = Hash.new
        iv_list = instance_variables
        SSL_IVNAMES.each_with_index do |ivname, i|
          if iv_list.include?(ivname) and
            value = instance_variable_get(ivname)
            ssl_parameters[SSL_ATTRIBUTES[i]] = value if value
          end
        end
        @ssl_context = OpenSSL::SSL::SSLContext.new
        @ssl_context.set_params(ssl_parameters)
        @ssl_context.session_cache_mode =
          OpenSSL::SSL::SSLContext::SESSION_CACHE_CLIENT |
          OpenSSL::SSL::SSLContext::SESSION_CACHE_NO_INTERNAL_STORE
        @ssl_context.session_new_cb = proc {|sock, sess| @ssl_session = sess }
        D "starting SSL for #{conn_address}:#{conn_port}..."
        s = OpenSSL::SSL::SSLSocket.new(s, @ssl_context)
        s.sync_close = true
        # Server Name Indication (SNI) RFC 3546
        # HERE WE ARE DISABLING SNI!
        # s.hostname = @address if s.respond_to? :hostname=
        if @ssl_session and
           Process.clock_gettime(Process::CLOCK_REALTIME) < @ssl_session.time.to_f + @ssl_session.timeout
          s.session = @ssl_session
        end
        ssl_socket_connect(s, @open_timeout)
        if @ssl_context.verify_mode != OpenSSL::SSL::VERIFY_NONE # nosemgrep
          s.post_connection_check(@address)
        end
        D "SSL established"
      end
      @socket = BufferedIO.new(s, read_timeout: @read_timeout,
                               continue_timeout: @continue_timeout,
                               debug_output: @debug_output)
      on_connect
    rescue => exception
      if s
        D "Conn close because of connect error #{exception}"
        s.close
      end
      raise
    end
  end
end
