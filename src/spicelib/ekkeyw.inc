! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
!
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
