-- Sprint
select vs.*
from v_sprint vs,
      (select compid
      from v_sprint_maj_int
      where rank <= {rnk}
      group by compid
      having count(distinct eventid) >= {topn}) tp
where vs.compid = tp.compid
