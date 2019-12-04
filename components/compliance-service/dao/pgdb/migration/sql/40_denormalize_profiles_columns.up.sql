CREATE INDEX IF NOT EXISTS store_profiles_info_name_version ON store_profiles ((info->>'name'), (info->>'version'));
CREATE INDEX IF NOT EXISTS store_profiles_info_title_version ON store_profiles ((info->>'title'), (info->>'version'));
