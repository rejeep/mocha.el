Feature: Run

  Background:
    Given I create directory "lib"
    And I create directory "test"
    And I create file "lib/awesome.js" with contents:
      """
      """
    And I create file "lib/not-so-awesome.js" with contents:
      """
      """
    And I create file "test/awesome.spec.js" with contents:
      """
      """
    And I create directory "node_modules"
    And I create file ".nvmrc" with contents:
      """
      0.10
      """

  Scenario: No node_modules directory
    Given I delete "node_modules"
    When I visit file "test/awesome.spec.js"
    And I run mocha
    Then I should see error "Could not determine project root, did not find any node_modules directory"

  Scenario: No .nvmrc file
    Given I delete ".nvmrc"
    When I visit file "test/awesome.spec.js"
    And I run mocha
    Then I should see error "Using Nvm, but did not find any .nvmrc file"

  Scenario: In test file
    When I visit file "test/awesome.spec.js"
    And I run mocha
    Then I should see buffer "awesome.spec.js"
    And I should see buffer "*mocha-compilation*"
    And I should see contents in buffer "*mocha-compilation*":
      """
      Mocha Compilation started
      """

  Scenario: In lib file
    When I visit file "lib/awesome.js"
    And I run mocha
    Then I should see buffer "awesome.js"
    And I should see buffer "*mocha-compilation*"
    And I should see contents in buffer "*mocha-compilation*":
      """
      Mocha Compilation started
      """

  Scenario: In lib file with no matching test file
    When I visit file "lib/not-so-awesome.js"
    And I run mocha
    Then I should see error "Did not find any matching test file for lib/not-so-awesome.js"
