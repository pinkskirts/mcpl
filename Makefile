SRC_DIR = ./src
OUT_DIR = ./out
MAIN_DIR = $(SRC_DIR)/com/mcpl/main/Main.java

JC = javac
JCFLAGS = -cp $(SRC_DIR) -d $(OUT_DIR)

default:
	$(JC) $(JCFLAGS) $(MAIN_DIR)

run:
	java -cp $(OUT_DIR) $(MAIN_DIR)
