INSERT INTO UPGRADE_FLAGS(
  id, upgrade_flag, upgrade_value, upgrade_time
) 
select 
  * 
from 
  (
    select 
      3 as id, 
      'enhanced_reporting' as upgrade_flag, 
      false as upgrade_value, 
      TO_TIMESTAMP(
        '2022-07-18 01:00:00', 'YYYY-MM-DD HH:MI:SS'
      ) as upgrade_time
  ) as tmp 
where 
  not exists (
    select 
      upgrade_flag 
    from 
      UPGRADE_FLAGS 
    where 
      upgrade_flag = 'enhanced_reporting' 
    limit 
      1
  );
