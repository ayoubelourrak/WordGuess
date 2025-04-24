package it.unipi.dsmt.word_guess;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.ServletComponentScan;

@SpringBootApplication
@ServletComponentScan
public class WordGuessApplication {

	public static void main(String[] args) {
		SpringApplication.run(WordGuessApplication.class, args);
	}

}
