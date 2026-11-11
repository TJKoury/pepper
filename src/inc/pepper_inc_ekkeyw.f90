!> The parameters of SPICELIB's include file ekkeyw.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekkeyw

   use pepper_kinds, only: ip

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

   integer(ip), parameter :: kwlen = 32

   integer(ip), parameter :: nkeywd = 29

   integer(ip), parameter :: kwall = 1

   integer(ip), parameter :: kwand = kwall + 1

   integer(ip), parameter :: kwasnd = kwand + 1

   integer(ip), parameter :: kwavg = kwasnd + 1

   integer(ip), parameter :: kwbetw = kwavg + 1

   integer(ip), parameter :: kwby = kwbetw + 1

   integer(ip), parameter :: kwcnt = kwby + 1

   integer(ip), parameter :: kwdsnd = kwcnt + 1

   integer(ip), parameter :: kwdstn = kwdsnd + 1

   integer(ip), parameter :: kweq = kwdstn + 1

   integer(ip), parameter :: kwfrom = kweq + 1

   integer(ip), parameter :: kwge = kwfrom + 1

   integer(ip), parameter :: kwgrp = kwge + 1

   integer(ip), parameter :: kwgt = kwgrp + 1

   integer(ip), parameter :: kwhav = kwgt + 1

   integer(ip), parameter :: kwis = kwhav + 1

   integer(ip), parameter :: kwle = kwis + 1

   integer(ip), parameter :: kwlike = kwle + 1

   integer(ip), parameter :: kwlt = kwlike + 1

   integer(ip), parameter :: kwmax = kwlt + 1

   integer(ip), parameter :: kwmin = kwmax + 1

   integer(ip), parameter :: kwne = kwmin + 1

   integer(ip), parameter :: kwnot = kwne + 1

   integer(ip), parameter :: kwnull = kwnot + 1

   integer(ip), parameter :: kwor = kwnull + 1

   integer(ip), parameter :: kwordr = kwor + 1

   integer(ip), parameter :: kwsel = kwordr + 1

   integer(ip), parameter :: kwsum = kwsel + 1

   integer(ip), parameter :: kwwher = kwsum + 1

!
!     End Include Section:  EK Keyword Code Parameters
!

end module pepper_inc_ekkeyw
