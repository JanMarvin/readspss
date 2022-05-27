
data list list / 
    d1 (DATE9) 
    d2 (DATE11) 
    a1 (ADATE8) 
    a2 (ADATE10)
    e1 (EDATE8)
    e2 (EDATE10)
    j1 (JDATE5)
    j2 (JDATE7)
    s1 (SDATE8)
    s2 (SDATE10)
    q1 (QYR6)
    q2 (QYR8)
    m1 (MOYR6)
    m2 (MOYR8)
    w1 (WKYR8)
    w2 (WKYR10)
    dt1 (DATETIME17)
    dt2 (DATETIME20)
    dt3 (DATETIME23.2)
    y1 (YMDHMS16)
    y2 (YMDHMS19)
    y3 (YMDHMS19) /* 19.2 .
    w3 (WKDAY3)
    w4 (WKDAY9)
    m3 (MONTH3)
    m4 (MONTH9)
    mt1 (MTIME5)	
    mt2 (MTIME8.2)	
    t1 (TIME5)
    t2 (TIME8)	
    t3 (TIME11.2)
    dt4 (DTIME9)	
    dt5 (DTIME12)	
    dt6 (DTIME15.2)
    .

begin data.
"31-JAN-13",  "31-JAN-2013", "01/31/13", "01/31/2013", "31.01.13", "31.01.2013", "13031", "2013031", "13/01/31", "2013/01/31", "1 Q 13", "1 Q 2013", "JAN 13", "JAN 2013", "5 WK 13", "5 WK 2013", "31-JAN-2013 01:02", "31-JAN-2013 01:02:33", "31-JAN-2013 01:02:33.72", "2013-01-31 1:02", "2013-01-31 1:02:33", "2013-01-31 1:02:33.72", "THU", "THURSDAY", "JAN", "JANUARY", "1754:36", "1754:36.58", "29:14", "29:14:36", "29:14:36.58", "1 05:14", "1 05:14:36", "1 05:14:36.58"
end data. 

save outfile = "/tmp/datetimes.sav" .