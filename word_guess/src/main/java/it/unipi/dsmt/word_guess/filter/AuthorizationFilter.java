package it.unipi.dsmt.word_guess.filter;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * Filter to check if user is authenticated for protected pages
 */
@WebFilter("/*")
public class AuthorizationFilter implements Filter {

    // List of paths that don't require authentication
    private static final List<String> PUBLIC_PATHS = Arrays.asList(
            "/login", "/register"
    );

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;

        String path = httpRequest.getRequestURI().substring(httpRequest.getContextPath().length());

        // Check if the requested path is public
        if (PUBLIC_PATHS.stream().anyMatch(path::startsWith)) {
            chain.doFilter(request, response);
            return;
        }

        // Check if user is logged in
        HttpSession session = httpRequest.getSession(false);
        boolean isLoggedIn = session != null && session.getAttribute("username") != null;

        if (isLoggedIn) {
            // User is authenticated, continue
            chain.doFilter(request, response);
        } else {
            // User is not authenticated, redirect to login
            httpResponse.sendRedirect(httpRequest.getContextPath() + "/login");
        }
    }
}