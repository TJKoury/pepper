! $ Abstract
!
!     This include file lists the parameter collection
!     defining the number of SPICE ID -> NAME mappings.
!
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
! $ Parameters
!
!     MAXL        is the maximum length of a body name.
!
!     MAXP        is the maximum number of additional names that may
!                 be added via the ZZBODDEF interface.
!
!     NPERM       is the count of the mapping assignments built into
!                 SPICE.
!
!     MAXE        is the size of the lists and hashes storing combined
!                 built-in and ZZBODDEF-defined name/ID mappings. To
!                 ensure efficient hashing this size is the set to the
!                 first prime number greater than ( MAXP + NPERM ).
!
!     NROOM       is the size of the lists and hashes storing the
!                 POOL-defined name/ID mappings. To ensure efficient
!                 hashing and to provide the ability to store nearly as
!                 many names as can fit in the POOL, this size is
!                 set to the first prime number less than MAXLIN
!                 defined in the POOL umbrella routine.
!
! $ Required_Reading
!
!     naif_ids.req
!
! $ Keywords
!
!     BODY
!     CONVERSION
!
! $ Author_and_Institution
!
!     B.V. Semenov (JPL)
!     E.D. Wright  (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 10-DEC-2021 (BVS)(EDW)
!
!        Increased NROOM to 14983. Added a comment note explaining
!        NROOM and MAXE
!
!-    SPICELIB Version 1.0.0, 20-MAY-2010 (EDW)
!
!        N0064 version with MAXP = 150, NPERM = 563,
!        MAXE = MAXP + NPERM, and NROOM = 2000.


!
!     A script generates this file. Do not edit by hand.
!     Edit the creation script to modify the contents of
!     ZZBODTRN.INC.
!

!
!     Maximum size of a NAME string
!
      INTEGER               MAXL
      PARAMETER           ( MAXL  =  36 )

!
!     Maximum number of additional names that may be added via the
!     ZZBODDEF interface.
!
      INTEGER               MAXP
      PARAMETER           ( MAXP  =  150 )

!
!     Count of default SPICE mapping assignments.
!
      INTEGER               NPERM
      PARAMETER           ( NPERM = 692 )

!
!     Size of the lists and hashes storing the built-in and
!     ZZBODDEF-defined name/ID mappings. To ensure efficient hashing
!     this size is the set to the first prime number greater than
!     ( MAXP + NPERM ).
!
      INTEGER               MAXE
      PARAMETER           ( MAXE  = 853 )

!
!     Size of the lists and hashes storing the POOL-defined name/ID
!     mappings. To ensure efficient hashing and to provide the ability
!     to store nearly as many names as can fit in the POOL, this size
!     is set to the first prime number less than MAXLIN defined in
!     the POOL umbrella routine.
!
      INTEGER               NROOM
      PARAMETER           ( NROOM = 14983 )

