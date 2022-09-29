module TomlRB
  module OffsetDateTimeParser
    def value
      skeleton = captures[:datetime_skeleton].first
      year, mon, day, hour, min, sec, sec_frac = skeleton.value
      offset = captures[:date_offset].first || "+00:00"
      sec = "#{sec}.#{sec_frac}".to_f

      Time.new(year, mon, day, hour, min, sec, offset.to_s)
    end
  end

  module LocalDateTimeParser
    def value
      year, mon, day = captures[:date_skeleton].first.value
      hour, min, sec, sec_frac = captures[:time_skeleton].first.value
      usec = sec_frac.to_s.ljust(6, "0")

      Time.local(year, mon, day, hour, min, sec, usec)
    end
  end

  module LocalDateParser
    def value
      year, mon, day = captures[:date_skeleton].first.value
      Time.local(year, mon, day)
    end
  end

  module LocalTimeParser
    def value
      hour, min, sec, sec_frac = captures[:time_skeleton].first.value
      usec = sec_frac.to_s.ljust(6, "0")

      Time.at(3600 * hour.to_i + 60 * min.to_i + sec.to_i, usec.to_i)
    end
  end
end
