!! TIME MODULE OF THE MODEL
!! refered from pom and roms
module mod_time
implicit none
private
!
!public :: TIMEFUN
public :: get_date
public :: time_string
public :: day_code
contains
!
!subroutine TIMEFUN(IY,IM,ID,IH,MIN,ISEC,hourtime)
!  IMPLICIT NONE
!! !TIMEE=TIMEFUN(IYEARE,IMONTHE,IDAYE,IHOURE,IMINUTEE,0) ! IN HOURS
!  REAL(kind=8) HOURTIME
!  INTEGER MN(12),MN1(12),IY,IM,ID,IH,MIN,Isec,iyy,iyy1
!  INTEGER IDD
!  DATA MN/0,31,59,90,120,151,181,212,243,273,304,334/
!  DATA MN1/0,31,60,91,121,152,182,213,244,274,305,335/
!  IDD=365*(IY-1)+(IY-1)/4-(IY-1)/100+(IY-1)/400
!  IYY=(IY-1)/4-(IY-1)/100+(IY-1)/400
!  IYY1=IY/4-IY/100+IY/400
!  IF(IYY1.GT.IYY) THEN
!    IDD=IDD+MN1(IM)+ID-1
!  ELSE
!    IDD=IDD+MN(IM)+ID-1
!  ENDIF
!  HOURTIME=0.24D2*IDD+.1D1*IH+MIN/0.6D2+ISEC/3600.  ! IN HOURS
!!! print*,TIMEFUN,IY,IM,ID,IH,MIN
!  RETURN
!  END subroutine TIMEFUN
!
SUBROUTINE get_date (date_str)
!!
!!svn $Id: get_date.F 751 2015-01-07 22:56:36Z arango $
!!================================================== Hernan G. Arango ===
!!  Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!    Licensed under a MIT/X style license                              !
!!    See License_ROMS.txt                                              !
!!=======================================================================
!!                                                                      !
!!   This routine gets todays date, day of the week and time called     !
!!  (default month & weekday are December & Saturday respectively).     !
!!  It uses SUN intrinsic date routine by default.                      !
!!                                                                      !
!!  On Output:                                                          !
!!                                                                      !
!!     date_str   Concatenated string for the day of the week, date     !
!!                (month,day,year), and time (12hr clock) of day        !
!!                (hour:min:sec).                                       !
!!                                                                      !
!!=======================================================================
!!
    use mod_constants, only : p2
!!
      implicit none
!
!  Imported variable declarations.
!
      character (len=*), intent(out) :: date_str
!
!  Local variable declarations.
!
      integer :: iyear, imonth, iday, ihour, iminute, isecond
      integer :: Dindex, i, half, len1, len2, len3
      integer, dimension(8) :: values
      integer, dimension(31) :: lday =                                  &
     &          (/ (1,i=1,9), (2,i=1,22) /)
      integer, dimension(12) :: lmonth =                                &
     &          (/ 7, 8, 5, 5, 3, 4, 4, 6, 9, 7, 8, 8 /)
      character (len= 5) :: czone
      character (len= 8) :: cdate
      character (len=10) :: ctime
      character (len=11) :: tstring
      character (len=18) :: today
      character (len=20) :: fmt
      character (len=44) :: dstring
      character (len=3), dimension(0:1) :: ampm =                       &
     &                   (/' AM',' PM'/)
      character (len=9), dimension(0:6) :: day =                        &
     &                   (/ 'Sunday   ','Monday   ','Tuesday  ',        &
     &                      'Wednesday','Thursday ','Friday   ',        &
     &                      'Saturday ' /)
      character (len=9), dimension(12) :: month =                       &
     &                   (/ 'January  ','February ','March    ',        &
     &                      'April    ','May      ','June     ',        &
     &                      'July     ','August   ','September',        &
     &                      'October  ','November ','December ' /)
!
!-----------------------------------------------------------------------
!  Get weekday, date and time in short format, then extract its
!  information.
!-----------------------------------------------------------------------
!
      CALL date_and_time (cdate, ctime, czone, values)
!
      iyear=values(1)            ! 4-digit year
      imonth=values(2)           ! month of the year
      iday=values(3)             ! day of the month
      ihour=values(5)            ! hour of the day, local time
      iminute=values(6)          ! minutes of the hour, local time
      isecond=values(7)          ! seconds of the minute, local time
!
!-----------------------------------------------------------------------
!  Convert from 24 hour clock to 12 hour AM/PM clock.
!-----------------------------------------------------------------------
!
      half=ihour/12
      ihour=ihour-half*12
      IF (ihour.eq.0) ihour=12
      IF (half.eq.2) half=0
!
!-----------------------------------------------------------------------
!  Get index for the day of the week.
!-----------------------------------------------------------------------
!
      CALL day_code (imonth, iday, iyear, Dindex)
!
!-----------------------------------------------------------------------
!  Construct date, time and day of the week output string.
!-----------------------------------------------------------------------
!
      WRITE (fmt,10) lmonth(imonth), lday(iday)
 10   FORMAT ('(a',i1,',1x,i',i1,',1h,,1x,i4)')
      WRITE (today,fmt) month(imonth), iday, iyear
      dstring=day(Dindex)
      WRITE (tstring,20) ihour, iminute, isecond, ampm(half)
 20   FORMAT (i2,':',i2.2,':',i2.2,a3)
!
!  Concatenate date string.
!
      len1=LEN_TRIM(dstring)
      len2=LEN_TRIM(today)
      len3=LEN_TRIM(tstring)
      date_str=TRIM(ADJUSTL(dstring(1:len1)))
      IF (len2.gt.0) THEN
        len1=LEN_TRIM(date_str)
        WRITE (date_str,'(a," - ",a)') TRIM(date_str(1:len1)),          &
     &                                 TRIM(today(1:len2))
      END IF
      IF (len3.gt.0) THEN
        len1=LEN_TRIM(date_str)
        WRITE (date_str,'(a," - ",a)') TRIM(date_str(1:len1)),          &
     &                                 TRIM(tstring(1:len3))
      END IF
      RETURN
      END SUBROUTINE get_date

       SUBROUTINE day_code (month, day, year, code)
!
!=======================================================================
!                                                                      !
!  This subroutine computes a code for the day of the week, given      !
!  the date. This code is good for date after:                         !
!                                                                      !
!                              January 1, 1752 AD                      !
!                                                                      !
!  the year the Gregorian calander was adopted in Britian and the      !
!  American colonies.                                                  !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     month     The month, 1=January, 2=February, ... (integer).       !
!     day       The day of the month (integer).                        !
!     year      The year, including the century (integer).             !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     code      A code for the corresponding day of the week           !
!                 (integer):                                           !
!                 code = 0  =>  Sunday                                 !
!                 code = 1  =>  Monday                                 !
!                 code = 2  =>  Tuesday                                !
!                 code = 3  =>  Wednesday                              !
!                 code = 4  =>  Thursday                               !
!                 code = 5  =>  Friday                                 !
!                 code = 6  =>  Saturday                               !
!                                                                      !
!=======================================================================
!
      use mod_constants, only : p2
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: month, day, year
      integer, intent(out) :: code
!
!  Local variable declarations.
!
      logical :: leap_flag
      integer, parameter :: base_cen = 1700
      integer, parameter :: base_qcen = 1600
      integer, parameter :: base_qyear = 1748
      integer, parameter :: base_year = 1752
      integer, parameter :: bym1_dec31 = 5
      integer, parameter :: feb_end = 59
      integer :: i, leap, no_day, no_yr, nqy, nyc, nyqc
      integer, dimension(12) :: month_day =                             &
     &         (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
!
!-----------------------------------------------------------------------
!  Compute the number of years since the base year, the number of
!  years since the beginning of the base century and the number of
!  years since the beginning of the base 400 year.
!-----------------------------------------------------------------------
!
      no_yr=year-base_year
      nqy=year-base_qyear
      nyc=year-base_cen
      nyqc=year-base_qcen
!
!-----------------------------------------------------------------------
!  Compute the number of leapdays in that time.  Determine if this
!  is a leap year.
!-----------------------------------------------------------------------
!
      leap=nqy/4-nyc/100+nyqc/400
      leap_flag=((MOD(nqy,4).eq.0).and.(MOD(nyc,100).ne.0)).or.         &
     &           (MOD(nyqc,400).eq.0)
!
!-----------------------------------------------------------------------
!  Compute the number of days this year.  The leap year corrections
!  are:
!        Jan. 1 - Feb. 28   Have not had the leap day counted above.
!        Feb.29             Counting leap day twice.
!-----------------------------------------------------------------------
!
      no_day=day
      DO i=1,month-1
        no_day=no_day+month_day(i)
      END DO
      IF (leap_flag.and.(no_day.le.feb_end))  no_day=no_day-1
      IF (leap_flag.and.(month.eq.2).and.(day.eq.29)) no_day=no_day-1
!
!-----------------------------------------------------------------------
!  Compute the total number of days since Jan. 1 of the base year,
!  exclusive of the 364 day per year which represent an even 52
!  weeks.  Actually, only need to do the addition mod 7.
!-----------------------------------------------------------------------
!
      no_day=MOD(no_day,7)+MOD(leap,7)+MOD(no_yr,7)+bym1_dec31
!
!-----------------------------------------------------------------------
!  Get the day of the week code.
!-----------------------------------------------------------------------
!
      code=MOD(no_day,7)
      RETURN
      END SUBROUTINE day_code

      SUBROUTINE time_string (time, time_code)
!
!=======================================================================
!                                                                      !
!  This routine encodes current model time in seconds to a time        !
!  string of the form:                                                 !
!                                                                      !
!       DDDDD HH:MM:SS                                                 !
!                                                                      !
!  This allow a more accurate label when reporting time.               !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     time       Current model time  (seconds)                         !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     time_code  Current model time string.                            !
!                                                                      !
!=======================================================================
!
      use mod_constants, only : p2
!
      implicit none
!
!  Imported variable declarations.
!
      real(p2), intent(in) :: time
      character(len=14), intent(out) :: time_code
!
!  Local variable declarations.
!
      integer :: iday, ihour, isec, minute
      character (len=14) :: text
!
!-----------------------------------------------------------------------
!  Encode current mode time.
!-----------------------------------------------------------------------
!
      iday=INT(time/86400)
      isec=INT(time-iday*86400.0_p2)
      ihour=isec/3600
      minute=MOD(isec,3600)/60
      isec=MOD(isec,60)

      WRITE (text,10) iday, ihour, minute, isec
 10   FORMAT (i5,1x,i2.2,':',i2.2,':',i2.2)
      time_code=text
      RETURN
      END SUBROUTINE time_string
  end module mod_time