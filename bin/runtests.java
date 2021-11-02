///usr/bin/env jbang "$0" "$@" ; exit $?
//DEPS org.junit.platform:junit-platform-console-standalone:1.8.1
//DEPS org.assertj:assertj-core:3.21.0
//DEPS org.mockito:mockito-junit-jupiter:3.6.0
import org.junit.platform.console.ConsoleLauncher;

/**
 * This script is mainly used to execute our tests standalone in CI with JDK 8.
 */
public class runtests {
	public static void main(String...a) {
		ConsoleLauncher.main(a);
	}
}
