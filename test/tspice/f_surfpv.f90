!$Procedure F_SURFPV ( SURFPV tests )

      SUBROUTINE F_SURFPV ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine SURFPV.
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
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routine SURFPV.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 2.0.1, 16-SEP-2020 (JDR)
!
!        Edited the header and test family comments to comply with
!        NAIF standard.
!
!-    TSPICE Version 2.0.0, 01-MAY-2012 (NJB)
!
!        Loosened "tight" tolerance to 1.e-10.
!
!-    TSPICE Version 1.1.0, 06-JAN-2009 (NJB)
!
!        Bug fix: regrouped terms of computation of
!        LEVEL to avoid overflow.
!
!-    TSPICE Version 1.0.0, 30-MAY-2008 (NJB)
!
!-&


! $ Index_Entries
!
!     Test family for SURFPV routine
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               ODD

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-10 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 400 )

      INTEGER               NSIMPL
      PARAMETER           ( NSIMPL =  8 )

      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 5000 )


!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(MSGLEN)    DESCR  ( NSIMPL )

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      SMPA   (     NSIMPL )
      DOUBLE PRECISION      SMPB   (     NSIMPL )
      DOUBLE PRECISION      SMPC   (     NSIMPL )
      DOUBLE PRECISION      SMPDST ( 6,  NSIMPL )
      DOUBLE PRECISION      SMPVST ( 6,  NSIMPL )
      DOUBLE PRECISION      STDIR  ( 6 )
      DOUBLE PRECISION      STVRTX ( 6 )
      DOUBLE PRECISION      STX    ( 6 )
      DOUBLE PRECISION      XSTX   ( 6 )

      INTEGER               I
      INTEGER               J
      INTEGER               SEED

      LOGICAL               FOUND
      LOGICAL               SMPFND ( NSIMPL )
      LOGICAL               XFOUND

!
!     Saved everything.
!
      SAVE

!
!     Initial values
!
!     The simple test cases.
!
      DATA DESCR(1) / &
       'Vertex moves; direction vector has zero velocity.' /

      DATA SMPA (1), SMPB(1), SMPC(1)  /   1.D2 , 1.D2,  1.D2  /

      DATA ( SMPVST(I,1), I = 1, 3 )   /   2.D2,  0.D0,  0.D0  /
      DATA ( SMPVST(I,1), I = 4, 6 )   /   0.D0,  0.D0,  1.D0  /
      DATA ( SMPDST(I,1), I = 1, 3 )   /  -1.D0,  0.D0,  0.D0  /
      DATA ( SMPDST(I,1), I = 4, 6 )   /   0.D0,  0.D0,  0.D0  /

      DATA   SMPFND(1)                 /   .TRUE.  /


      DATA DESCR(2) / &
       'Direction vector scans upward; vertex has zero velocity.' /

      DATA SMPA (2), SMPB(2), SMPC(2)  /   1.D2 , 2.D2,  3.D2  /

      DATA ( SMPVST(I,2), I = 1, 3 )   /   2.D2,  0.D0,  0.D0  /
      DATA ( SMPVST(I,2), I = 4, 6 )   /   0.D0,  0.D0,  0.D0  /
      DATA ( SMPDST(I,2), I = 1, 3 )   /  -1.D0,  0.D0,  0.D0  /
      DATA ( SMPDST(I,2), I = 4, 6 )   /   0.D0,  0.D0,  1.D0  /

      DATA   SMPFND(2)                 /   .TRUE.  /


      DATA DESCR(3) / &
       'No intersection; ray passes above target.' /

      DATA SMPA (3), SMPB(3), SMPC(3)  /   1.D2 , 2.D2,  3.D2   /

      DATA ( SMPVST(I,3), I = 1, 3 )   /   2.D2,  0.D0,  3.1D2  /
      DATA ( SMPVST(I,3), I = 4, 6 )   /   0.D0,  0.D0,  0.D0   /
      DATA ( SMPDST(I,3), I = 1, 3 )   /  -1.D0,  0.D0,  0.D0   /
      DATA ( SMPDST(I,3), I = 4, 6 )   /   0.D0,  0.D0,  1.D0   /

      DATA   SMPFND(3)                 /   .FALSE.  /


      DATA DESCR(4) / &
       'No intersection; ray points away from target.' /

      DATA SMPA (4), SMPB(4), SMPC(4)  /   1.D2 , 2.D2,  3.D2   /

      DATA ( SMPVST(I,4), I = 1, 3 )   /   2.D2,  0.D0,  0.D0  /
      DATA ( SMPVST(I,4), I = 4, 6 )   /   0.D0,  0.D0,  0.D0   /
      DATA ( SMPDST(I,4), I = 1, 3 )   /   1.D0,  0.D0,  0.D0   /
      DATA ( SMPDST(I,4), I = 4, 6 )   /   0.D0,  0.D0,  1.D0   /

      DATA   SMPFND(4)                 /   .FALSE.  /


      DATA DESCR(5) / &
       'Velocity not computable: ray scans upward too fast.'  /
      DATA SMPA (5), SMPB(5), SMPC(5)  /   1.D0 , 2.D0,  3.D0  /

      DATA ( SMPVST(I,5), I = 1, 3 )   /   2.D0,  0.D0,  0.D0  /
      DATA ( SMPVST(I,5), I = 4, 6 )   /   0.D0,  0.D0,  0.D0  /
      DATA ( SMPDST(I,5), I = 1, 3 )   /  -1.D0,  0.D0,  0.D0  /
      DATA ( SMPDST(I,5), I = 4, 6 )   /   0.D0,  0.D0,  1.D308  /

      DATA   SMPFND(5)                 /   .FALSE.  /


      DATA DESCR(6) / &
       'Velocity not computable: vertex moves upward too fast.'  /
      DATA SMPA (6), SMPB(6), SMPC(6)  /   1.D0 , 2.D0,  3.D0  /

      DATA ( SMPVST(I,6), I = 1, 3 )   /   2.D0,  0.D0,  2.0D0   /
      DATA ( SMPVST(I,6), I = 4, 6 )   /   0.D0,  0.D0,  1.5D308 /
      DATA ( SMPDST(I,6), I = 1, 3 )   /  -1.D0,  0.D0,  0.D0    /
      DATA ( SMPDST(I,6), I = 4, 6 )   /   0.D0,  0.D0,  0.D0    /

      DATA   SMPFND(6)                 /   .FALSE.  /


      DATA DESCR(7) / &
       'Interior vertex; vertex not at origin.' /

      DATA SMPA (7), SMPB(7), SMPC(7)  /   1.D2 , 2.D2,  3.D2   /

      DATA ( SMPVST(I,7), I = 1, 3 )   /   1.D1,  0.D0,  0.D0  /
      DATA ( SMPVST(I,7), I = 4, 6 )   /   0.D0,  0.D0,  0.D0   /
      DATA ( SMPDST(I,7), I = 1, 3 )   /   1.D0,  0.D0,  0.D0   /
      DATA ( SMPDST(I,7), I = 4, 6 )   /   0.D0,  0.D0,  1.D0   /

      DATA   SMPFND(7)                 /   .TRUE.  /


      DATA DESCR(8) / &
       'Interior vertex; vertex is at origin.' /

      DATA SMPA (8), SMPB(8), SMPC(8)  /   1.D2 , 2.D2,  3.D2   /

      DATA ( SMPVST(I,8), I = 1, 3 )   /   0.D0,  0.D0,  0.D0   /
      DATA ( SMPVST(I,8), I = 4, 6 )   /   0.D0,  0.D0,  0.D0   /
      DATA ( SMPDST(I,8), I = 1, 3 )   /   1.D0,  0.D0,  0.D0   /
      DATA ( SMPDST(I,8), I = 4, 6 )   /   0.D0,  0.D0,  1.D0   /

      DATA   SMPFND(8)                 /   .TRUE.  /


!
!     Open the test family.
!
      CALL TOPEN ( 'F_SURFPV' )


!
!     *****************************************************************
!
!     Error cases: SURFPV
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE  ( 'SURFPV: zero direction vector,' )

      CALL FILLD  ( 1.D1, 6,  STVRTX )
      CALL CLEARD (       6,  STDIR  )
      CALL SURFPV   ( STVRTX, STDIR,  A, B, C, STX,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE  ( 'SURFPV: ellipsoid has one zero-length axis.' )

      CALL FILLD (   1.D1, 6, STVRTX )
      CALL FILLD (  -1.D0, 6, STDIR  )

      CALL SURFPV   ( STVRTX, STDIR, 0.D0, 1.D0, 1.D0, STX,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFPV   ( STVRTX, STDIR, 1.D0, 0.D0, 1.D0, STX,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFPV   ( STVRTX, STDIR, 1.D0, 1.D0, 0.D0, STX,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE  ( 'SURFPV: ellipsoid has one negative axis.' )

      CALL FILLD (   1.D1, 6, STVRTX )
      CALL FILLD (  -1.D0, 6, STDIR  )

      CALL SURFPV   ( STVRTX, STDIR, -1.D0, 1.D0, 1.D0, STX,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFPV   ( STVRTX, STDIR, 1.D0, -1.D0, 1.D0, STX,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFPV   ( STVRTX, STDIR, 1.D0, 1.D0, -1.D0, STX,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE  ( 'SURFPV: ray''s vertex is on the ellipsoid' )

      CALL FILLD (   0.D0, 3, STVRTX )
      STVRTX(1) = 1.D0
      CALL FILLD (   1.D0, 3, STVRTX(4) )

      CALL FILLD (  -1.D0, 6, STDIR  )

      CALL SURFPV   ( STVRTX, STDIR, 1.D0, 1.D0, 1.D0, STX, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVERTEX)', OK )

      CALL T_SURFPV ( STVRTX, STDIR, 1.D0, 1.D0, 1.D0, STX, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVERTEX)', OK )


!
!     *****************************************************************
!
!     Normal cases: SURFPV
!
!     *****************************************************************
!
!     Run some simple tests where the correct results can be
!     determined by inspection.
!
      DO I = 1, NSIMPL

!
!        --- Case: ----------------------------------------------------
!
         CALL TCASE  ( DESCR(I) )


         CALL SURFPV ( SMPVST(1,I),  SMPDST(1,I), &
                       SMPA(I),      SMPB(I),    SMPC(I), &
                       STX,          FOUND                 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND (0)', FOUND, SMPFND(I), OK )

!         IF ( FOUND ) THEN
!            WRITE (*,*) 'STX from SURFPV: ', STX
!         END IF

!
!        Repeat the computation with T_SURFPV.
!
         CALL T_SURFPV ( SMPVST(1,I),  SMPDST(1,I), &
                         SMPA(I),      SMPB(I),    SMPC(I), &
                         XSTX,         FOUND                 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND (1)', FOUND, SMPFND(I), OK )

         IF ( SMPFND(I) ) THEN
!
!           Check the intercept state against that found
!           by T_SURFPV.
!
            CALL CHCKAD ( 'STX(1:3)', STX, '~~/', XSTX, 3, TIGHT, OK )

            CALL CHCKAD ( 'STX(4:6)', STX(4), '~~/', &
                                      XSTX(4), 3, TIGHT, OK )

         END IF

      END DO


!
!     Now for some more difficult cases.  We'll generate the ellipsoids
!     and lines using random numbers.  There are 16 components to
!     generate:
!
!        - random ray vertices and vertex velocities
!        - random ray directions and direction velocities
!        - random ellipsoid axis lengths
!        - random scale factors for the ellipsoid and ray
!
      SEED = -1


      DO I = 1, NRANDM
!
!        --- Case: ---------------------------------------------------
!
!
!        Get a scale factor.
!
         SFACTR    =  10.D0 ** T_RANDD ( -290.D0, 290.D0, SEED )

!
!        Make up ellipsoid axis lengths.
!
         A         =  SFACTR  *  T_RANDD (  1.D0,   2.D0, SEED )
         B         =  SFACTR  *  T_RANDD (  1.D0,   2.D0, SEED )
         C         =  SFACTR  *  T_RANDD (  1.D0,   2.D0, SEED )

!
!        We gotta have a ray vertex and vertex velocity.
!
         DO J = 1, 6
            STVRTX(J) =  10 * SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         END DO

!
!        Compute the level surface parameter of the vertex.
!
         LEVEL =     ( STVRTX(1) / A )**2 &
                  +  ( STVRTX(2) / B )**2 &
                  +  ( STVRTX(3) / C )**2

!
!        Alternate between exterior and interior cases.
!
         IF ( ODD(I) ) THEN

!
!           If necessary, scale up the vertex to take it outside the
!           ellipsoid.
!
            IF ( LEVEL .LT. 1.D0 ) THEN

               IF ( LEVEL .GT. 0.D0 ) THEN

                  CALL VSCLIP ( 2.D0/SQRT(LEVEL), STVRTX )

               ELSE

!
!                 It's unlikely that we get here: the vertex is at
!                 the origin. Pick an exterior vertex.
!
                  CALL VPACK ( 2.D0*A, 2.D0*B, 2.D0*C, STVRTX )

               END IF

            END IF

         ELSE

!
!           Ensure the vertex is inside the ellipsoid.
!
            IF ( LEVEL .GE. 1.D0 ) THEN

               CALL VSCLIP ( 0.5D0/SQRT(LEVEL), STVRTX )

            END IF

         END IF

!
!        Generate the ray's direction vector by perturbing the
!        inverse of the ray's vertex.
!
         DO J = 1, 3
            STDIR(J) =   - STVRTX(J) &
                         + SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         END DO

         DO J = 4, 6
            STDIR(J) =     SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         END DO


         TITLE = 'SURFPV Random case #.  A, B, C = # # #; ' &
         //      'STVRTX = (#, #, #, #, #, #) ' &
         //      'STDIR  = (#, #, #, #, #, #)'
         CALL REPMI ( TITLE, '#', I,             TITLE )
         CALL REPMD ( TITLE, '#', A,         14, TITLE )
         CALL REPMD ( TITLE, '#', B,         14, TITLE )
         CALL REPMD ( TITLE, '#', C,         14, TITLE )
         CALL REPMD ( TITLE, '#', STVRTX(1), 14, TITLE )
         CALL REPMD ( TITLE, '#', STVRTX(2), 14, TITLE )
         CALL REPMD ( TITLE, '#', STVRTX(3), 14, TITLE )
         CALL REPMD ( TITLE, '#', STVRTX(4), 14, TITLE )
         CALL REPMD ( TITLE, '#', STVRTX(5), 14, TITLE )
         CALL REPMD ( TITLE, '#', STVRTX(6), 14, TITLE )
         CALL REPMD ( TITLE, '#', STDIR (1), 14, TITLE )
         CALL REPMD ( TITLE, '#', STDIR (2), 14, TITLE )
         CALL REPMD ( TITLE, '#', STDIR (3), 14, TITLE )
         CALL REPMD ( TITLE, '#', STDIR (4), 14, TITLE )
         CALL REPMD ( TITLE, '#', STDIR (5), 14, TITLE )
         CALL REPMD ( TITLE, '#', STDIR (6), 14, TITLE )

         CALL TCASE ( TITLE )

!
!        Cross our fingers and toes and let 'er rip.
!
         CALL SURFPV   ( STVRTX, STDIR,  A, B, C, STX,  FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL T_SURFPV ( STVRTX, STDIR,  A, B, C, XSTX, XFOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND (0)', FOUND, XFOUND, OK )

         IF ( XFOUND ) THEN

!
!           Check the intercept state against that found
!           by T_SURFPV.
!
            CALL CHCKAD ( 'STX(1:3)', STX, '~~/', XSTX, 3, TIGHT, OK )

            CALL CHCKAD ( 'STX(4:6)', STX(4), '~~/', &
                                      XSTX(4), 3, TIGHT, OK )

!            WRITE (*,*) '-----------------'
!            WRITE (*,*) 'Case ', I
!            WRITE (*,*) 'STX:  ', STX
!            WRITE (*,*) 'XSTX: ', XSTX

         END IF


      END DO


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
