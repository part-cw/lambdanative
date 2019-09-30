
NAME=LNjScheme

android_jars/jscheme.jar: $(NAME)/*.java
	@-mkdir android_jars
	rm -f $(NAME)/*.class
	javac $(NAME)/Scheme.java
	jar cf $@ $(NAME)/*.class
