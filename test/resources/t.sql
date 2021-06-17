select a.c1, a.c2, b.c4, b.c56 as coln3, y.c67 as colname1, y.c100 
from tab1 as a
inner join tab2 as b
on a.c11 = b.c11 and a.c12 = b.c12
left join (select * from tab3 as x -- where c44 = 99 group by c35 order by c34
inner join (select * from tab31 where c1 = 45) as y
on x.c1 = y.c1 and x.c2 = y.c2
) as e
on b.c22 = e.c22 
where c3 = 44 and c4 = 'open' and c5 = 66
group by c1, c2, c3, abs(c4), c5
order by c1, c2, c3, abs(c4), c5
