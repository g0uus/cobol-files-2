
TARGET = files2
COBOL_FLAGS = -ffreeform -free -Wall -Wextra -Wno-unused-variable -Wno-unused-function

all: ${TARGET}

files2: ${TARGET}.cbl updates-rec.cpy
	gcobol -o ${TARGET} ${TARGET}.cbl

run: ${TARGET}
	./${TARGET}

clean:
	rm -rf ${TARGET}