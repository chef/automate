CREATE OR REPLACE FUNCTION test_upsert_notification_subscription()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
    test_enterprise CONSTANT enterprises.name%TYPE = 'BigCo';
    test_organization CONSTANT organizations.name%TYPE = 'BigCo Engineering';
    test_project CONSTANT projects.name%TYPE = 'skunkworks';
    test_user CONSTANT users.name%TYPE = 'BigCo Chaos Monkey';
    test_categories CONSTANT TEXT[] = '{review}';
    test_categories2 CONSTANT TEXT[] = '{review, observe}';
    test_categories3 CONSTANT TEXT[] = '{}';

    test_subscription_id notification_subscriptions.id%TYPE;
    test_subscription_id2 notification_subscriptions.id%TYPE;
    test_subscription_id3 notification_subscriptions.id%TYPE;
    test_subscription notification_subscriptions;
BEGIN
  --Create notification_subscription
  SELECT id
  FROM upsert_notification_subscription(
       test_enterprise,
       test_organization,
       test_project,
       test_user,
       test_categories) INTO test_subscription_id;

  -- should have created a subscription
  SELECT *
  FROM notification_subscriptions
  WHERE id = test_subscription_id
  INTO test_subscription;
  RETURN QUERY SELECT is(
    test_subscription.categories,
    '{review}'
  );

  --Update notification_subscription
  SELECT id
  FROM upsert_notification_subscription(
       test_enterprise,
       test_organization,
       test_project,
       test_user,
       test_categories2) INTO test_subscription_id2;

  -- should have updated a subscription's categories
  SELECT *
  FROM notification_subscriptions
  WHERE id = test_subscription_id2
  INTO test_subscription;
  RETURN QUERY SELECT is(
    test_subscription.categories,
    '{review, observe}'
  );

  --Delete notification_subscription by removing categories
  SELECT id
  FROM upsert_notification_subscription(
       test_enterprise,
       test_organization,
       test_project,
       test_user,
       test_categories3) INTO test_subscription_id3;

  SELECT *
  FROM notification_subscriptions
  WHERE id = test_subscription_id3
  INTO test_subscription;
  RETURN QUERY SELECT is(
    test_subscription.id,
    NULL
  );

END;
$$;

