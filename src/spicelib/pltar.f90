!$Procedure PLTAR ( Compute area of plate set )

      DOUBLE PRECISION FUNCTION PLTAR ( NV, VRTCES, NP, PLATES )

! $ Abstract
!
!     Compute the total area of a collection of triangular plates.
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
!     DSK
!
! $ Keywords
!
!     DSK
!     GEOMETRY
!     MATH
!     TOPOGRAPHY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               NV
      DOUBLE PRECISION      VRTCES ( 3, NV )
      INTEGER               NP
      INTEGER               PLATES ( 3, NP )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NV         I   Number of vertices.
!     VRTCES     I   Array of vertices.
!     NP         I   Number of triangular plates.
!     PLATES     I   Array of plates.
!
!     The function returns the total area of the set of plates.
!
! $ Detailed_Input
!
!     NV       is the number of vertices comprising the plate
!              set.
!
!     VRTCES   is an array containing the plate model's vertices.
!              Elements
!
!                 VRTCES( 1, I )
!                 VRTCES( 2, I )
!                 VRTCES( 3, I )
!
!              are, respectively, the X, Y, and Z components of
!              the Ith vertex.
!
!              This routine doesn't associate units with the
!              vertices.
!
!     NP       is the number of triangular plates comprising the
!              plate set.
!
!     PLATES   is an array containing 3-tuples of integers
!              representing the set of plates. The elements of
!              PLATES are vertex indices. The vertex indices are
!              1-based: vertices have indices ranging from 1 to
!              NV. The elements
!
!                 PLATES( 1, I )
!                 PLATES( 2, I )
!                 PLATES( 3, I )
!
!              are, respectively, the indices of the vertices
!              comprising the Ith plate.
!
!              Note that the order of the vertices of a plate is
!              significant: the vertices must be ordered in the
!              positive (counterclockwise) sense with respect to
!              the outward normal direction associated with the
!              plate. In other words, if V1, V2, V3 are the
!              vertices of a plate, then
!
!                 ( V2 - V1 )  x  ( V3 - V2 )
!
!              points in the outward normal direction. Here
!              "x" denotes the vector cross product operator.
!
! $ Detailed_Output
!
!     The function returns the total area of the input set of plates.
!     Each plate contributes the area of the triangle defined by the
!     plate's vertices.
!
!     If the components of the vertex array have length unit L, then the
!     output area has units
!
!         2
!        L
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the number of plates is less than 0, the error
!         SPICE(BADPLATECOUNT) is signaled.
!
!     2)  If the number of plates is positive and the number of vertices
!         is less than 3, the error SPICE(TOOFEWVERTICES) is signaled.
!
!     3)  If any plate contains a vertex index outside of the range
!
!            [1, NV]
!
!         the error SPICE(INDEXOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes the total area of a set of triangular
!     plates. The plates need not define a closed surface.
!
!     Examples of valid plate sets:
!
!        Tetrahedron
!        Box
!        Tiled ellipsoid
!        Tiled ellipsoid with one plate removed
!        Two disjoint boxes
!        Two boxes with intersection having positive volume
!        Single plate
!        Empty plate set
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input
!     (if any), the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!
!     1) Compute the area of the pyramid defined by the four
!        triangular plates whose vertices are the 3-element
!        subsets of the set of vectors:
!
!           ( 0, 0, 0 )
!           ( 1, 0, 0 )
!           ( 0, 1, 0 )
!           ( 0, 0, 1 )
!
!
!        Example code begins here.
!
!
!              PROGRAM PLTAR_EX1
!              IMPLICIT NONE
!        C
!        C     Compute the area of a plate model representing the
!        C     pyramid with one vertex at the origin and the other
!        C     vertices coinciding with the standard basis vectors.
!        C
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      PLTAR
!        C
!        C     Local parameters
!        C
!              INTEGER               NVERT
!              PARAMETER           ( NVERT  = 4 )
!
!              INTEGER               NPLATE
!              PARAMETER           ( NPLATE = 4 )
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      VRTCES ( 3, NVERT  )
!              DOUBLE PRECISION      AREA
!
!              INTEGER               PLATES ( 3, NPLATE )
!        C
!        C     Initial values
!        C
!        C     The plates defined below lie in the following planes,
!        C     respectively:
!        C
!        C        Plate 1:    { P :  < P, (-1,  0,  0) > = 0 }
!        C        Plate 2:    { P :  < P, ( 0, -1,  0) > = 0 }
!        C        Plate 3:    { P :  < P, ( 0,  0, -1) > = 0 }
!        C        Plate 4:    { P :  < P, ( 1,  1,  1) > = 1 }
!        C
!              DATA                  PLATES /    1,     4,     3,
!             .                                  1,     2,     4,
!             .                                  1,     3,     2,
!             .                                  2,     3,     4 /
!
!              DATA                  VRTCES / 0.D0,  0.D0,  0.D0,
!             .                               1.D0,  0.D0,  0.D0,
!             .                               0.D0,  1.D0,  0.D0,
!             .                               0.D0,  0.D0,  1.D0 /
!
!
!              AREA = PLTAR ( NVERT, VRTCES, NPLATE, PLATES )
!
!              WRITE (*,*) 'Expected area   =    (3 + SQRT(3)) / 2'
!              WRITE (*,*) '                =    0.2366025403784438E+01'
!              WRITE (*,*) 'Computed area   = ', AREA
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Expected area   =    (3 + SQRT(3)) / 2
!                         =    0.2366025403784438E+01
!         Computed area   =    2.3660254037844384
!
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
!-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard. Added DSK to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 21-OCT-2016 (NJB)
!
!        Original version 25-MAR-2016 (NJB)
!
!-&


! $ Index_Entries
!
!     compute plate model area
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      VNORM
      LOGICAL               RETURN


!
!     Local variables
!
      DOUBLE PRECISION      CP    ( 3 )
      DOUBLE PRECISION      EDGE1 ( 3 )
      DOUBLE PRECISION      EDGE2 ( 3 )

      INTEGER               I
      INTEGER               J


!
!     The function must have an initial value.
!
      PLTAR  =  0.D0

!
!     This routine uses discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Check the vertex and plate counts.
!
      IF ( NP .LT. 0 ) THEN

         CALL CHKIN  ( 'PLTAR'                                        )
         CALL SETMSG ( 'Plate count must be non-negative but NP = #.' )
         CALL ERRINT ( '#',  NP                                       )
         CALL SIGERR ( 'SPICE(BADPLATECOUNT)'                         )
         CALL CHKOUT ( 'PLTAR'                                        )
         RETURN

      END IF


      IF ( NP .EQ. 0 ) THEN
!
!        The area has already been set to zero.
!
         RETURN

      END IF


      IF ( NV .LT. 3 ) THEN

         CALL CHKIN  ( 'PLTAR'                                       )
         CALL SETMSG ( 'At least 3 vertices are needed, but NV = #.' )
         CALL ERRINT ( '#',  NV                                      )
         CALL SIGERR ( 'SPICE(TOOFEWVERTICES)'                       )
         CALL CHKOUT ( 'PLTAR'                                       )
         RETURN

      END IF

!
!     Make sure the vertex indices are in the range [1, NV].
!
      DO I = 1, NP

         DO J = 1, 3

            IF (      ( PLATES(J,I) .LT. 1  ) &
                 .OR. ( PLATES(J,I) .GT. NV )  ) THEN

               CALL CHKIN  ( 'PLTAR'                                   )
               CALL SETMSG ( 'Vertex indices must be in the range ' &
               //            '[1, NV] for all SPICE language versions. ' &
               //            'The input value of NV was #. Vertex ' &
               //            'index # in plate # was #. (The vertex ' &
               //            'and plate numbers in this message are ' &
               //            '1-based as well.)'                       )
               CALL ERRINT ( '#',  NV                                  )
               CALL ERRINT ( '#',  J                                   )
               CALL ERRINT ( '#',  I                                   )
               CALL ERRINT ( '#',  PLATES(J,I)                         )
               CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                  )
               CALL CHKOUT ( 'PLTAR'                                   )
               RETURN

            END IF

         END DO

      END DO


      DO I = 1, NP
!
!        Take the cross product of two edges of the Ith plate.
!
         CALL VSUB ( VRTCES( 1, PLATES(2,I) ), &
                     VRTCES( 1, PLATES(1,I) ),   EDGE1 )

         CALL VSUB ( VRTCES( 1, PLATES(3,I) ), &
                     VRTCES( 1, PLATES(2,I) ),   EDGE2 )

         CALL VCRSS ( EDGE1, EDGE2, CP )

!
!        The plate area is 1/2 of the magnitude of the
!        cross product.
!
         PLTAR = PLTAR  +  ( 0.5D0 * VNORM(CP) )

      END DO

!
!     No check-out required, since the routine is not checked in
!     at this point.
!
      RETURN
      END
