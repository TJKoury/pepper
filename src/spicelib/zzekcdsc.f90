!$Procedure  ZZEKCDSC ( Private: EK, return column descriptor )

      SUBROUTINE ZZEKCDSC ( HANDLE, SEGDSC, COLUMN, COLDSC )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Look up the column descriptor for a column of a given name
!     in a specified segment.
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
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      CHARACTER*(*)         COLUMN
      INTEGER               COLDSC ( CDSCSZ )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to an EK file.
!     SEGDSC     I   Segment descriptor.
!     COLUMN     I   Name of column.
!     COLDSC     O   Descriptor for specified column.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle for the file containing the
!                    column of interest.  The EK may be open for read
!                    or write access.
!
!     SEGDSC         is the descriptor of the segment containing the
!                    column for which a descriptor is desired.
!
!     COLUMN         is the name of the column whose descriptor is
!                    desired.  Case and white space are not significant.
!
! $ Detailed_Output
!
!     COLDSC         is the descriptor of the column belonging to the
!                    specified file and segment and having name COLUMN.
!                    See the include file ekcoldsc.inc for details
!                    regarding the structure of EK column descriptors.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input column name does not match any column in the
!         designated segment, the error SPICE(BUG) is signaled.  It
!         is the caller's responsibility to call this routine with
!         valid input arguments.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine exists for the sole purpose of centralizing code
!     used to perform column descriptor look-ups.
!
! $ Examples
!
!     See the EKACEx routines.
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error message.
!
!-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               EQSTR

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    CNAME

      INTEGER               DSCBAS
      INTEGER               I
      INTEGER               MBASE
      INTEGER               NAMBAS
      INTEGER               NCOLS

      LOGICAL               FOUND

!
!     Use discovery check-in.
!
!     Get the segment's integer metadata's base address.
!
      MBASE  =  SEGDSC ( IMDIDX )

!
!     Get the number of columns.
!
      NCOLS  =  SEGDSC ( NCIDX )

!
!     Search linearly through the column descriptors, looking for
!     a column name match.  It's an error if we don't find the input
!     name.
!
      FOUND = .FALSE.
      I     =  1

      DO WHILE (  ( I .LE. NCOLS )  .AND.  ( .NOT. FOUND )  )

         DSCBAS  =  MBASE + SDSCSZ + (I-1)*CDSCSZ
!
!        Get the character base address of the column name from the
!        current descriptor.
!
         CALL DASRDI ( HANDLE, DSCBAS+1, DSCBAS+CDSCSZ, COLDSC )

         NAMBAS = COLDSC( NAMIDX )

!
!        Look up the name and compare.
!
         CALL DASRDC ( HANDLE,  NAMBAS+1,  NAMBAS+CNAMSZ, &
                       1,       CNAMSZ,    CNAME         )

         IF (  EQSTR( CNAME, COLUMN )  ) THEN
            FOUND = .TRUE.
         ELSE
            I     =  I + 1
         END IF


      END DO


      IF ( .NOT. FOUND ) THEN

         CALL CHKIN  ( 'ZZEKCDSC'                                 )
         CALL SETMSG ( 'Descriptor for column # was not found. ' // &
                       'Segment base = #; file = #.'              )
         CALL ERRCH  ( '#',  COLUMN                               )
         CALL ERRINT ( '#',  MBASE                                )
         CALL ERRHAN ( '#',  HANDLE                               )
         CALL SIGERR ( 'SPICE(BUG)'                               )
         CALL CHKOUT ( 'ZZEKCDSC'                                 )
         RETURN

      END IF

      END
