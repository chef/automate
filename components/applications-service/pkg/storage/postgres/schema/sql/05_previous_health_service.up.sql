-- Add previous_health column
ALTER TABLE service ADD COLUMN previous_health TEXT NOT NULL DEFAULT '';
