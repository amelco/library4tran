program lib4tran
implicit none

! change to false if compilling to WINDOWS =(
logical, parameter              :: linux = .true.
character(10), parameter        :: configFile = 'config.txt'   ! name of config file
                                                               ! config file stores: libPath, ... (to be added)

logical                 :: configExist  ! TRUE if configuration file already exists
character(20)           :: libFile      ! file that contains all information about the PDF files
character(120)          :: libPath      ! Path to PDF files
character               :: choice       ! User choice (main menu)
logical                 :: invalidChoice



! -- Beginning of the program
! -- clear screen
call clearScr()
! -- writes header
call header()
! -- check if config file exists
inquire(file=configFile, exist=configExist)
if (configExist.eqv..false.) then
  call createConfigFile()
else
  call getConfigs()
endif

do 
  ! -- clear screen
  call clearScr()
  ! -- writes header
  call header()
  call menu()
  if (invalidChoice) then
    write(*,*) 'Invalid choice!'
  endif
  invalidChoice = .false.
  write(*,'(A)', advance='no') 'Your choice: '
  read(*,'(A1)') choice
  select case (choice)
    case ('1')
      call search()
    case ('l')
      call list()
    case ('q')
      call quit()
    case default
      invalidChoice = .true.
  end select

enddo

contains

subroutine search()
  implicit none
  character(1024)       :: buffer
  integer               :: pos, posant, eof, i
  character(240)        :: title, id, filename, tipo, author, year
  character(50)         :: string

  print*
  write(*,'(A)', advance='no') 'Type your search string (title, author, year):  '
  read(*,'(A50)') string
  open(10, file=libFile, status='old')
  read(10,*)    ! First line is header
  i=1
  eof=0
  do while (eof.ne.-1)
    read(10,'(A)', iostat=eof) buffer
    !print*, index(buffer, trim(string))
    if (index(buffer, trim(string)) .gt. 0) then
      if (i == 1 .or. mod(i,39).eq.0) then
        if (i==1) then
          print*
        else
          write(*,'(A4,A140,A3)') '|  ', '', '  |'
        endif
        write(*,'(A4,A4,A10,A8,A80,3x,A25,3x,A7,A3)') '|  ', 'id', 'filename', 'type', 'title', 'author', 'year', '  |'
      endif
      pos=1
      posant=pos
      pos = index(buffer, ';')
      id = buffer(posant:pos-1)
      posant = pos+1
      pos = posant-1 + index(buffer(posant:), ';')
      filename = buffer(posant:pos-1)
      posant = pos+1
      pos = posant-1 + index(buffer(posant:), ';')
      tipo = buffer(posant:pos-1)
      posant = pos+1
      pos = posant-1 + index(buffer(posant:), ';')
      title = buffer(posant:pos-1)
      posant = pos+1
      pos = posant-1 + index(buffer(posant:), ';')
      author = buffer(posant:pos-1)
      year = buffer(pos+1:)
      write(*,'(A4,A4,A10,A8,A80,3x,A25,3x,A7,A3)') '|  ', id, filename, tipo, title, author, year, '  |'
      i = i + 1
    endif
    

  enddo
  print*
  call pressEnter()
  close(10)

end subroutine

subroutine list()
  implicit none
  character(1024)       :: buffer
  integer               :: pos, posant, eof, i
  character(240)        :: title, id, filename, tipo, author, year

  open(10, file=libFile, status='old')
  read(10,*)    ! First line is header
  
  i=1
  eof=0
  write(*,'(A4,A4,A10,A8,A80,3x,A5,3x,A7,A3)') '|  ', 'id', 'filename', 'type', 'title', 'author', 'year', '  |'
  do while (eof.ne.-1)
    read(10,'(A)', iostat=eof) buffer
    !print*, buffer
    if (mod(i,39).eq.0) then    ! reprint header at each 39 lines
      write(*,'(A4,A120,A3)') '|  ', '', '  |'
      write(*,'(A4,A4,A10,A8,A80,3x,A5,3x,A7,A3)') '|  ', 'id', 'filename', 'type', 'title', 'author', 'year', '  |'
      write(*,*) ' ----------------------------------------------------------------'&
      &'----------------------------------------------------------- '
    endif
    pos=1
    posant=pos
    pos = index(buffer, ';')
    id = buffer(posant:pos-1)
    posant = pos+1
    pos = posant-1 + index(buffer(posant:), ';')
    filename = buffer(posant:pos-1)
    posant = pos+1
    pos = posant-1 + index(buffer(posant:), ';')
    tipo = buffer(posant:pos-1)
    posant = pos+1
    pos = posant-1 + index(buffer(posant:), ';')
    title = buffer(posant:pos-1)
    posant = pos+1
    pos = posant-1 + index(buffer(posant:), ';')
    author = buffer(posant:pos-1)
    year = buffer(pos+1:)
    
    write(*,'(A4,A4,A10,A8,A80,3x,A5,3x,A7,A3)') '|  ', id, filename, tipo, title, author, year, '  |'
    i = i + 1
  enddo
  print*
  call pressEnter()
  close(10)
end subroutine

subroutine quit()
  stop
end subroutine

subroutine menu()
  implicit none
  print*
  write(*,*) ' -== MENU ==-'
  print*
  write(*,*) ' 1) Search'
  write(*,*) ' l) List all papers'
  write(*,*) ' q) Quit'
  print*
end subroutine

subroutine createConfigFile()
  implicit none
  logical       :: libExist
  character     :: choice, bar
  integer       :: i

  if (linux) then
    bar = '/'
  else
    bar = '\'
  endif
  print*
  write(*,*) 'Configuration file not found. Answer the following questions to create it.'
  print*
  write(*,*) 'What is the FULL path to your PDF files?'
  read(*,'(A)') libPath
  
  ! -- adding / to the end of libPath string if the user didn't
  i = index(libPath,' ')
  if (libPath(i-1:i-1) .ne. bar) then
    libPath(i:i) = '/'
  endif
  
  write(*,*) 'Which databse (.dat) file do you want to use (default: mylib.dat)'
  read(*,'(A)') libFile
  inquire(file=trim(libFile), exist=libExist)
  if (libExist) then
    write(*,*) trim(libfile)//' already exists. Do you want to replace it (y/n)?'
    read(*,*) choice
    if (choice.ne.'y') then
      write(*,*) 'Enter new filename for the databse (.dat) file (default: mylib.dat)'
      read(*,'(A)') libFile
    endif
  endif
  if (trim(libFile) .eq. '') then
    libFile = 'mylib.dat'
  endif
  open(10, file='config.txt', status='replace')
  write(10,'(A)') '* Name of database file'
  write(10,'(A)') trim(libFile)
  write(10,'(A)') '* Full path to PDF files'
  write(10,'(A)') trim(libPath)
  close(10)
  print*
  write(*,*) 'Configuration file created!'
  call pressEnter()
end subroutine

subroutine pressEnter()
  implicit none
  write(*,*) '(Press ENTER to continue)'
  read*
end subroutine

subroutine getConfigs()
  implicit none
  open(10, file='config.txt', status='old')
  read(10,*)
  read(10,'(A)') libFile
  read(10,*)
  read(10,'(A)') libPath
  close(10)
end subroutine

subroutine header()
  implicit none
  write(*,*) '   *** Library 4tran - academic papers manager ***'
  print*
end subroutine

subroutine clearScr()
  implicit none
  if (linux) then
    call system('clear')
  else
    call system('cls')
  endif
end subroutine

end program
