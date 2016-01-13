package studententrend;

import org.hibernate.cfg.Configuration;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
@EnableAutoConfiguration
public class Application {
	
	/**
	 * Starts the Sprint boot application
	 * @param args
	 */
	public static void main(String[] args) {
		Configuration cfg = new Configuration();
		cfg.addResource("hibernate.cfg.xml"); 
		
		SpringApplication.run(Application.class, args);
	}
}
