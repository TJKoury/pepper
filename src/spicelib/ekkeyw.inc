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

      INTEGER               KWLEN
      PARAMETER           ( KWLEN  =   32 )

      INTEGER               NKEYWD
      PARAMETER           ( NKEYWD =   29 )


      INTEGER               KWALL
      PARAMETER           ( KWALL  =          1 )

      INTEGER               KWAND
      PARAMETER           ( KWAND  = KWALL  + 1 )

      INTEGER               KWASND
      PARAMETER           ( KWASND = KWAND  + 1 )

      INTEGER               KWAVG
      PARAMETER           ( KWAVG  = KWASND + 1 )

      INTEGER               KWBETW
      PARAMETER           ( KWBETW = KWAVG  + 1 )

      INTEGER               KWBY
      PARAMETER           ( KWBY   = KWBETW + 1 )

      INTEGER               KWCNT
      PARAMETER           ( KWCNT  = KWBY   + 1 )

      INTEGER               KWDSND
      PARAMETER           ( KWDSND = KWCNT  + 1 )

      INTEGER               KWDSTN
      PARAMETER           ( KWDSTN = KWDSND + 1 )

      INTEGER               KWEQ
      PARAMETER           ( KWEQ   = KWDSTN + 1 )

      INTEGER               KWFROM
      PARAMETER           ( KWFROM = KWEQ   + 1 )

      INTEGER               KWGE
      PARAMETER           ( KWGE   = KWFROM + 1 )

      INTEGER               KWGRP
      PARAMETER           ( KWGRP  = KWGE   + 1 )

      INTEGER               KWGT
      PARAMETER           ( KWGT   = KWGRP  + 1 )

      INTEGER               KWHAV
      PARAMETER           ( KWHAV  = KWGT   + 1 )

      INTEGER               KWIS
      PARAMETER           ( KWIS   = KWHAV  + 1 )

      INTEGER               KWLE
      PARAMETER           ( KWLE   = KWIS   + 1 )

      INTEGER               KWLIKE
      PARAMETER           ( KWLIKE = KWLE   + 1 )

      INTEGER               KWLT
      PARAMETER           ( KWLT   = KWLIKE + 1 )

      INTEGER               KWMAX
      PARAMETER           ( KWMAX  = KWLT   + 1 )

      INTEGER               KWMIN
      PARAMETER           ( KWMIN  = KWMAX  + 1 )

      INTEGER               KWNE
      PARAMETER           ( KWNE   = KWMIN  + 1 )

      INTEGER               KWNOT
      PARAMETER           ( KWNOT  = KWNE   + 1 )

      INTEGER               KWNULL
      PARAMETER           ( KWNULL = KWNOT  + 1 )

      INTEGER               KWOR
      PARAMETER           ( KWOR   = KWNULL + 1 )

      INTEGER               KWORDR
      PARAMETER           ( KWORDR = KWOR   + 1 )

      INTEGER               KWSEL
      PARAMETER           ( KWSEL  = KWORDR + 1 )

      INTEGER               KWSUM
      PARAMETER           ( KWSUM  = KWSEL  + 1 )

      INTEGER               KWWHER
      PARAMETER           ( KWWHER = KWSUM  + 1 )

!
!     End Include Section:  EK Keyword Code Parameters
!
