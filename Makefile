NASM 		= nasm
NFLAGS		= -f elf64
LDFLAGS		= -s -static --omagic
SOURCES		= f64.nasm
OBJECTS		= $(SOURCES:.nasm=.o)
TARGET		= f64
INCLUDES	= unistd_64.inc
LINKER		= ld

all: $(SOURCES) $(TARGET)
f64.o: f64.nasm unistd_64.inc
	$(NASM) $(NFLAGS) f64.nasm
$(TARGET): $(OBJECTS) $(INCLUDES) Makefile
	$(LINKER) -s $(OBJECTS) -o $@ $(LDFLAGS)
clean:
	rm -rf $(OBJECTS) $(TARGET)
install /usr/include/f64/f64.fs: f64.fs f64
	sudo mkdir -p /usr/include/f64
	sudo cp f64.fs /usr/include/f64
test: $(TARGET) /usr/include/f64/f64.fs primes.fs
	cat primes.fs | ./f64
