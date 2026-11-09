!> The parameters of SPICELIB's include file ekkeyw.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekkeyw

   implicit none

   public

!     Include Section:  EK Keyword Code Parameters
!
!        ekkeyw.inc  Version 4    24-JAN-1995 (NJB)
!
!
!
!     The EK query language keywords and codes are:
!
!        ALL
!        AND
!        ASC
!        AVG
!        BETWEEN
!        BY
!        COUNT
!        DESC
!        DISTINCT
!        EQ
!        FROM
!        GE
!        GROUP
!        GT
!        HAVING
!        IS
!        LE
!        LT
!        LIKE
!        MAX
!        MIN
!        NE
!        NOT
!        NULL
!        OR
!        ORDER
!        SELECT
!        SUM
!        WHERE
!

   integer               kwlen
   parameter           ( kwlen  =   32 )

   integer               nkeywd
   parameter           ( nkeywd =   29 )


   integer               kwall
   parameter           ( kwall  =          1 )

   integer               kwand
   parameter           ( kwand  = kwall  + 1 )

   integer               kwasnd
   parameter           ( kwasnd = kwand  + 1 )

   integer               kwavg
   parameter           ( kwavg  = kwasnd + 1 )

   integer               kwbetw
   parameter           ( kwbetw = kwavg  + 1 )

   integer               kwby
   parameter           ( kwby   = kwbetw + 1 )

   integer               kwcnt
   parameter           ( kwcnt  = kwby   + 1 )

   integer               kwdsnd
   parameter           ( kwdsnd = kwcnt  + 1 )

   integer               kwdstn
   parameter           ( kwdstn = kwdsnd + 1 )

   integer               kweq
   parameter           ( kweq   = kwdstn + 1 )

   integer               kwfrom
   parameter           ( kwfrom = kweq   + 1 )

   integer               kwge
   parameter           ( kwge   = kwfrom + 1 )

   integer               kwgrp
   parameter           ( kwgrp  = kwge   + 1 )

   integer               kwgt
   parameter           ( kwgt   = kwgrp  + 1 )

   integer               kwhav
   parameter           ( kwhav  = kwgt   + 1 )

   integer               kwis
   parameter           ( kwis   = kwhav  + 1 )

   integer               kwle
   parameter           ( kwle   = kwis   + 1 )

   integer               kwlike
   parameter           ( kwlike = kwle   + 1 )

   integer               kwlt
   parameter           ( kwlt   = kwlike + 1 )

   integer               kwmax
   parameter           ( kwmax  = kwlt   + 1 )

   integer               kwmin
   parameter           ( kwmin  = kwmax  + 1 )

   integer               kwne
   parameter           ( kwne   = kwmin  + 1 )

   integer               kwnot
   parameter           ( kwnot  = kwne   + 1 )

   integer               kwnull
   parameter           ( kwnull = kwnot  + 1 )

   integer               kwor
   parameter           ( kwor   = kwnull + 1 )

   integer               kwordr
   parameter           ( kwordr = kwor   + 1 )

   integer               kwsel
   parameter           ( kwsel  = kwordr + 1 )

   integer               kwsum
   parameter           ( kwsum  = kwsel  + 1 )

   integer               kwwher
   parameter           ( kwwher = kwsum  + 1 )

!
!     End Include Section:  EK Keyword Code Parameters
!

end module pepper_inc_ekkeyw
