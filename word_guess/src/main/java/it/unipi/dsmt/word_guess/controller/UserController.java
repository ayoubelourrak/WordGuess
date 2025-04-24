package it.unipi.dsmt.word_guess.controller;

import it.unipi.dsmt.word_guess.model.User;
import it.unipi.dsmt.word_guess.service.ApiService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import jakarta.servlet.http.HttpSession;
import java.util.Map;

@Controller
public class UserController {

    private final ApiService apiService;

    @Autowired
    public UserController(ApiService apiService) {
        this.apiService = apiService;
    }

    /**
     * Show the login page
     */
    @GetMapping("/login")
    public String showLoginForm(Model model, HttpSession session) {
        // If already logged in, redirect to game list
        if (session.getAttribute("username") != null) {
            return "redirect:/games";
        }

        model.addAttribute("user", new User());
        return "login";
    }

    /**
     * Process login form submission
     */
    @PostMapping("/login")
    public String processLogin(@ModelAttribute User user, HttpSession session, RedirectAttributes redirectAttributes) {
        Map<String, Object> response = apiService.loginUser(user);

        if ((boolean) response.get("success")) {
            // Store user data in session
            session.setAttribute("username", user.getUsername());

            return "redirect:/games";
        } else {
            // Add error message and return to login form
            redirectAttributes.addFlashAttribute("error", response.get("error"));
            return "redirect:/login";
        }
    }

    /**
     * Show the registration page
     */
    @GetMapping("/register")
    public String showRegisterForm(Model model, HttpSession session) {
        // If already logged in, redirect to game list
        if (session.getAttribute("username") != null) {
            return "redirect:/games";
        }

        model.addAttribute("user", new User());
        return "register";
    }

    /**
     * Process registration form submission
     */
    @PostMapping("/register")
    public String processRegistration(@ModelAttribute User user, RedirectAttributes redirectAttributes) {
        Map<String, Object> response = apiService.registerUser(user);

        if ((boolean) response.get("success")) {
            // Registration successful, redirect to login with success message
            redirectAttributes.addFlashAttribute("message", "Registration successful. Please log in.");
            return "redirect:/login";
        } else {
            // Add error message and return to registration form
            redirectAttributes.addFlashAttribute("error", response.get("error"));
            return "redirect:/register";
        }
    }

    /**
     * Process logout
     */
    @GetMapping("/logout")
    public String logout(HttpSession session) {
        // Invalidate session
        session.invalidate();
        return "redirect:/login";
    }
}