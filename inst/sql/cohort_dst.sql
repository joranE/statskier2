select vd.*
from v_distance vd,
      (select compid
      from v_distance_maj_int
      where rank <= {rnk} and age <= {age}
      group by compid
      having count(distinct eventid) >= {topn}) tp
where vd.compid = tp.compid
