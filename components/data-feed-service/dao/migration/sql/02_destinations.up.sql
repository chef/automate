ALTER TABLE destinations ADD CONSTRAINT name_not_empty CHECK(name <> '');
ALTER TABLE destinations ADD CONSTRAINT url_not_empty CHECK(url <> '');
