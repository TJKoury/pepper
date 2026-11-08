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
!     Include Section:  EK File Metadata Parameters
!
!        ekfilpar.inc  Version 1  28-MAR-1995 (NJB)
!
!     These parameters apply to EK files using architecture 4.
!     These files use a paged DAS file as their underlying file
!     structure.
!
!     The metadata for an architecture 4 EK file is very simple:  it
!     consists of a single integer, which is a pointer to a tree
!     that in turn points to the segments in the EK.  However, in the
!     interest of upward compatibility, one integer page is reserved
!     for the file's metadata.
!
!
!     Size of file parameter block:
!
      INTEGER               FPARSZ
      PARAMETER           ( FPARSZ = 1 )

!
!     All offsets shown below are relative to the beginning of the
!     first integer page in the EK.
!
!
!     Index of the segment pointer tree---this location contains the
!     root page number of the tree:
!
      INTEGER               SGTIDX
      PARAMETER           ( SGTIDX  =  1 )

!
!     End Include Section:  EK File Metadata Parameters
!
