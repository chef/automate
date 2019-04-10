CREATE INDEX IF NOT EXISTS idxprofileinfoname ON store_profiles ((info->>'name'));
CREATE INDEX IF NOT EXISTS idxprofileinfoversion ON store_profiles ((info->>'version'));
