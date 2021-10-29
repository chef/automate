-- Remove id as pkey to allow only uniq constraints to be effective
alter table public.orgs drop constraint orgs_pkey;
