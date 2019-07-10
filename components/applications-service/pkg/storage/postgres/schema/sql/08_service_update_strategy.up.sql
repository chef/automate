-- Add update_strategy column with a default of 'NONE'
--
-- Even though there will be services listening to a channel, we can't know what
-- strategy they are configure so we will wait for new messages to populate this field
ALTER TABLE service ADD COLUMN update_strategy TEXT NOT NULL DEFAULT 'NONE';
