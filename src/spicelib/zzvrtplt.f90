!$Procedure  ZZVRTPLT  ( create vertex-plate mapping )

      SUBROUTINE ZZVRTPLT ( NV,    NP,     PLATES, CELLSZ, MAXLST, &
                            CELLS, VRTPTR, NLIST,  PLTLST          )
      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Generate a data structure that represents the mapping from
!     vertices to the plates containing them.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     PLATE
!     VERTEX
!
! $ Declarations

      INCLUDE              'dsk02.inc'

      INTEGER               NV
      INTEGER               NP
      INTEGER               PLATES ( 3, * )
      INTEGER               CELLSZ
      INTEGER               MAXLST
      INTEGER               CELLS  ( 2, CELLSZ )
      INTEGER               VRTPTR ( * )
      INTEGER               NLIST
      INTEGER               PLTLST ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NV         I   Number of vertices.
!     NP         I   Number of plates.
!     PLATES     I   Plate array.
!     CELLSZ     I   Cell array size.
!     MAXLST     I   Maximum size of output plate list.
!     CELLS     I-O  Workspace array for vertex list construction.
!     VRTPTR     O   Array of pointers, by vertex, into plate data list.
!     NLIST      O   Length of the plate data list.
!     PLTLST     O   Plate data list; first element is the number of
!                    plate IDs to follow.
!
! $ Detailed_Input
!
!     NV         Number of vertices.
!
!     NP         Number of plates.
!
!     PLATES     Plate array: this is an array of 3-tuples of
!                vertex indices. The elements
!
!                   PLATES(1:3,I)
!
!                are the indices of the vertices of the Ith plate.
!
!
!     CELLSZ     Size of cell array: explicit workspace dimension used
!                for error checking. This value should be
!
!                   3*NP
!
!     MAXLST     Maximum size of output plate list. This size should be
!
!                   3*NP  +  NV
!
!
!     CELLS      workspace array used to construct the vertex-plate
!                mapping.
!
! $ Detailed_Output
!
!     CELLS      Workspace array used to construct the vertex-plate
!                mapping.
!
!     VRTPTR     Array of pointers associating plate lists with
!                vertices. The Ith element of VRTPTR contains
!                the start index in PLTLST of the list associated with
!                the Ith vertex.
!
!                The size of this array must be at least NV.
!
!     NLIST      Length of the output plate data list PLTLST.
!
!     PLTLST     Plate data list: for each vertex, there is a count
!                of associated plates, followed by the IDs of those
!                plates. The count for vertex I is located at
!
!                   PLTLST( VRTPTR(I) )
!
!                The size of this array must be at least MAXLST.
!
! $ Parameters
!
!     See parameter declarations in
!
!        dsk02.inc
!
! $ Exceptions
!
!     1)  If the input plate count NP is non-positive, the
!         error SPICE(BADPLATECOUNT) is signaled.
!
!     2)  If the input vertex count NV is non-positive, the
!         error SPICE(BADVERTEXCOUNT) is signaled.
!
!     3)  If the cell array size CELLSZ is less than
!
!            3 * NP
!
!         the error SPICE(CELLARRAYTOOSMALL) is signaled.
!
!     4)  If the plate list size MAXPLT is less than
!
!            ( 3 * NP ) + NV
!
!         the error SPICE(PLATELISTTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports the DSK type 2 spatial index
!     routines
!
!        DSKMI2
!        DSKSI2
!
! $ Examples
!
!     See usage in DSKMI2.
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
!     N.J. Bachman    (JPL)
!     J.A. Bytof      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-NOV-2020 (NJB)
!
!        Bug fix: in call to ZZADDLNK, size of pointer array was
!        changed from NP to NV.
!
!-    SPICELIB Version 1.0.0, 20-MAY-2016 (NJB)
!
!        Updated error handling. Now ZZVRTPLT performs precise checks
!        on cell and plate array size inputs.
!
!
!        22-DEC-2015 (NJB)
!
!           Renamed routine from VRTCOM to ZZVRTPLT.
!
!           CAUTION: argument list change! Removed input workspace
!           array for pointer construction. Array was named PNTRS.
!
!           Added error checks for input counts and sizes.
!
!           Now calls updated 3.0.0 version of ZZUNTNGL.
!
!        03-MAY-2014 (NJB)
!
!           Now calls ZZ* versions of supporting routines.
!
!           Changed argument list to include sizes of arrays.
!           Changed error handling to make use of array sizes.
!           Changed call to UNTNGL to accommodate argument
!           list change in that routine. Updated header I/O
!           descriptions.
!
!        12-JUL-2011 (NJB)
!
!           Argument list change: the input arrays V1, V2, V3 were
!           replaced with the input array PLATES.
!
!        04-MAY-2010 (NJB)
!
!           Now accepts input workspace arrays PNTRS and CELLS.
!           Changed INCLUDE file to dsk02.inc.
!
!        08-OCT-2009 (NJB)
!
!           Re-ordered header sections.
!
!        04-FEB-1999 (JAB)
!
!-&

! $ Index_Entries
!
!     create vertex to plate map
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local variables.
!

      LOGICAL               RETURN

      INTEGER               I
      INTEGER               J
      INTEGER               NCELL


      SAVE

!
!     Standard SPICELIB error handling.
!

      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZVRTPLT' )


      IF ( NV .LT. 1 ) THEN

         CALL SETMSG ( 'Vertex count NV = #; count must be positive.' &
         //            'be positive.'                                )
         CALL ERRINT ( '#',  NV                                      )
         CALL SIGERR ( 'SPICE(BADVERTEXCOUNT)'                       )
         CALL CHKOUT ( 'ZZVRTPLT'                                    )
         RETURN

      END IF

      IF ( NP .LT. 1 ) THEN

         CALL SETMSG ( 'Plate count NP = #; count must be positive.' &
         //            'be positive.'                                )
         CALL ERRINT ( '#',  NP                                      )
         CALL SIGERR ( 'SPICE(BADPLATECOUNT)'                        )
         CALL CHKOUT ( 'ZZVRTPLT'                                    )
         RETURN

      END IF

      IF ( CELLSZ .LT. 3*NP ) THEN

         CALL SETMSG ( 'Cell array size CELLSZ = #; size must be ' &
         //            '>= 3*NP. NP is the plate count #.'         )
         CALL ERRINT ( '#',  CELLSZ                                )
         CALL ERRINT ( '#',  NP                                    )
         CALL SIGERR ( 'SPICE(CELLARRAYTOOSMALL)'                  )
         CALL CHKOUT ( 'ZZVRTPLT'                                  )
         RETURN

      END IF

      IF ( MAXLST .LT. (NV+3*NP) ) THEN

         CALL SETMSG ( 'Plate list array size MAXPLT = #; size ' &
         //            'must be >= 3*NP + NV, which is #. (NV = ' &
         //            'vertex count, NP = plate count.)'        )
         CALL ERRINT ( '#',  MAXLST                              )
         CALL ERRINT ( '#',  3*NP + NV                           )
         CALL SIGERR ( 'SPICE(PLATELISTTOOSMALL)'                )
         CALL CHKOUT ( 'ZZVRTPLT'                                )
         RETURN

      END IF

!
!     Initialize pointer and cell structure.
!
      CALL ZZINILNK ( NV, CELLSZ, NCELL, VRTPTR, CELLS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZVRTPLT' )
         RETURN
      END IF

!
!     Loop over all plate IDS. Add each plate/vertex
!     combination to the linked list.
!
      DO I = 1, NP

         DO J = 1, 3
!
!           AVAL = PLATES(J,I), vertex J of plate ID I.
!           BVAL = I, plate ID value I.
!
            CALL ZZADDLNK ( PLATES(J,I), I,     NV,   CELLSZ, &
                            VRTPTR,      NCELL, CELLS        )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZVRTPLT' )
               RETURN
            END IF

         END DO

      END DO

!
!     Generate two linked lists mapping vertex ID to the plates
!     including that vertex as a member.
!
!     VRTPTR: An array, indexed by vertex ID. For an array element,
!             VRTPTR(VERT_ID), greater than zero, the value identifies
!             an index in PLTLST, the value of that PLTLST array
!             element  equaling the number of plates that include
!             the vertex specified by the ID as a member. The
!             condition VRTPTR(VERT_ID) = -1 indicates a bug.
!
!     PLTLST: An array, indexed by the positive entries in VRTPTR.
!             The element, N, identified by a VRTPTR value describes
!             the number of plates of which the vertex is a member.
!
!                 N = PLTLST( VRTPTR(VERT_ID) )
!
!             The N elements following PLTLST( VRTPTR(VERT_ID) ),
!             contain the IDs of those plates which have the vertex
!             as a member.
!
      CALL ZZUNTNGL ( NV,     CELLSZ, CELLS, &
                      MAXLST, VRTPTR, NLIST, PLTLST )

!
!     Standard SPICE error handling.
!
      CALL CHKOUT ( 'ZZVRTPLT' )
      RETURN
      END
