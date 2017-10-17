select
  m.*
from
  main m,
  (
    select
      gender,
      type,
      compid,
      fisid,
      count(distinct raceid) as n_result
    from
      maj_int
    where
      type != 'Stage' and
      rank <= @rank and
      fispoints is not null
    group by
      gender,
      type,
      compid,
      fisid
    having
      count(distinct raceid) >= @n_result
  ) mi
where
  m.gender = mi.gender and
  m.compid = mi.compid and
  m.fisid = mi.fisid and
  m.type != 'Stage' and
  m.fispoints is not null
