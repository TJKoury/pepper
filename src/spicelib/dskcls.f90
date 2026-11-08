!$Procedure DSKCLS ( DSK, close file )

      SUBROUTINE DSKCLS ( HANDLE, OPTMIZ )

! $ Abstract
!
!     Close a DSK file.
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
!     DAS
!     DSK
!
! $ Keywords
!
!     DAS
!     DSK
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'

      INTEGER               HANDLE
      LOGICAL               OPTMIZ

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle assigned to the opened DSK file.
!     OPTMIZ     I   Flag indicating whether to segregate the DSK.
!
! $ Detailed_Input
!
!     HANDLE   is the DAS file handle associated with the file.
!              The file may be open for read or write access.
!
!     OPTMIZ   is a logical flag indicating whether the DSK
!              should be segregated before it is closed. This
!              option applies only to files open for write
!              access. The value of OPTMIZ has no effect for
!              files opened for read access.
!
!              See the DAS Required Reading das.req for a
!              discussion of segregation of DAS files.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs when the file is closed, the error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     This routine provides a DSK-level interface for closing DSK files.
!
!     In cases where DSKs opened for write access are to be closed
!     without segregation, this interface is slightly simpler than that
!     available at the DAS level.
!
! $ Examples
!
!     1) Close a new DSK file using DAS segregation. HANDLE
!        is the DAS file handle of the DSK.
!
!        This is the normal choice for DSK creation.
!
!           CALL DSKCLS ( HANDLE, .TRUE. )
!
!     2) Close a new DSK file without using DAS segregation. The
!        close operation will be fast, but reading the file will be
!        less efficient than if the file had been segregated.
!
!           CALL DSKCLS ( HANDLE, .TRUE. )
!
!     3) Close an existing DSK file that had been opened
!        for read access. In this case OPTMIZ is ignored:
!
!           CALL DSKCLS ( HANDLE, .FALSE. )
!
!        or
!
!           CALL DSKCLS ( HANDLE, .TRUE. )
!
! $ Restrictions
!
!     1)  This routine should not be called by user applications
!         that have loaded a DSK file via FURNSH. Such applications
!         should call the KEEPER entry points UNLOAD or KCLEAR instead.
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
!-    SPICELIB Version 1.1.0, 12-OCT-2021 (JDR) (NJB)
!
!        Bug fix: now calls FAILED after call to DASHAM.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!
!        09-OCT-2009 (NJB)
!
!           Updated header.
!
!        20-OCT-2006 (NJB)
!
!           Original DSKLIB version.
!
!-&


! $ Index_Entries
!
!     close a DSK file
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               METHLN
      PARAMETER           ( METHLN  = 10 )

!
!     Local variables
!
      CHARACTER*(METHLN)    METHOD


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DSKCLS' )

      IF ( OPTMIZ ) THEN
!
!        Segregate the file to enable fast read access.  This is
!        the "normal" way to close a DSK.  Segregating a large file
!        can be slow, however.
!
         CALL DASCLS ( HANDLE )

      ELSE
!
!        Close the file without first segregating it; this allows
!        the caller to close the file quickly, but results in a
!        file that will be read more slowly.
!
!        Any buffered data to be written must be explicitly flushed
!        to the file, if the file is open for write access.
!
         CALL DASHAM ( HANDLE, METHOD )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DSKCLS' )
            RETURN
         END IF

         IF ( METHOD .EQ. 'WRITE ' ) THEN
!
!           Write out any buffered records belonging to the
!           indicated file.
!
            CALL DASWBR ( HANDLE )

         END IF

!
!        Close the file without segregating records.
!
         CALL DASLLC ( HANDLE )

      END IF

      CALL CHKOUT ( 'DSKCLS' )
      RETURN
      END
