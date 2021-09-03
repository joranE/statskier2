-- Sprint
select vs.*
from v_sprint vs,
      (select compid
      from v_sprint_maj_int
      where rank <= {rnk} and age <= {age}
      group by compid
      having count(distinct eventid) >= {topn}) tp
where vs.compid = tp.compid
