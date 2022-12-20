/*
 * Copyright (c) 2019-2023 "Neo4j,"
 * Neo4j Sweden AB [https://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.neo4j.cypherdsl.core;

import static com.tngtech.archunit.base.DescribedPredicate.not;
import static com.tngtech.archunit.core.domain.JavaAccess.Predicates.targetOwner;
import static com.tngtech.archunit.core.domain.JavaClass.Predicates.assignableTo;
import static com.tngtech.archunit.core.domain.JavaClass.Predicates.resideInAPackage;
import static com.tngtech.archunit.core.domain.JavaClass.Predicates.simpleName;
import static com.tngtech.archunit.core.domain.properties.HasModifiers.Predicates.modifier;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.fields;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses;

import org.apiguardian.api.API;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.neo4j.cypherdsl.core.internal.SchemaNamesBridge;
import org.neo4j.cypherdsl.support.schema_name.SchemaNames;

import com.tngtech.archunit.base.DescribedPredicate;
import com.tngtech.archunit.core.domain.JavaClass;
import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.domain.JavaModifier;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.lang.ArchRule;
import com.tngtech.archunit.lang.conditions.ArchConditions;

/**
 * @author Michael J. Simons
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class PackageAndAPIStructureTest {

	private JavaClasses coreClasses;

	// tag::arch-rules.naming:TypeNameMustBeginWithGroupId[]
	@BeforeAll
	void importCorePackage() {
		coreClasses = new ClassFileImporter()
			.withImportOption(ImportOption.Predefined.DO_NOT_INCLUDE_TESTS)
			.importPackages("org.neo4j.cypherdsl.core..");
	}
	// end::arch-rules.naming:TypeNameMustBeginWithGroupId[]

	@DisplayName("API Guardian annotations must not be used on fields")
	// tag::arch-rules.api:api-guardian-usage[]
	@Test
	void apiGuardian() {
		ArchRule rule = fields().should().notBeAnnotatedWith(API.class);
		rule.check(coreClasses);
	}
	// end::arch-rules.api:api-guardian-usage[]

	@DisplayName("Non abstract, public classes that are only part of internal API must be final and not reside in core")
	// tag::arch-rules.api:internal[]
	@Test
	void internalPublicClassesMustBeFinal() {
		ArchRule rule = classes().that()
			.areAnnotatedWith(API.class)
			.and().arePublic()
			.and().areTopLevelClasses()
			.and(not(modifier(JavaModifier.ABSTRACT)))
			.and(new DescribedPredicate<JavaClass>("Is internal API") {
				@Override
				public boolean test(JavaClass input) {
					API.Status status = input.getAnnotationOfType(API.class).status();
					return "INTERNAL".equals(status.name());
				}
			})
			.should().haveModifier(JavaModifier.FINAL)
			.andShould(ArchConditions.not(ArchConditions.resideInAPackage("..core")));
		rule.check(coreClasses);
	}
	// end::arch-rules.api:internal[]

	@DisplayName("The Cypher-DSL core package must not depend on the rendering infrastructure")
	// tag::arch-rules.structure:core-must-not-depend-on-renderer[]
	@Test
	void coreMostNotDependOnRendering() {
		ArchRule rule = noClasses().that()
			.resideInAPackage("..core")
			.and(not(simpleName("AbstractStatement")))
			.should().dependOnClassesThat(resideInAPackage("..renderer.."));
		rule.check(coreClasses);
	}
	// end::arch-rules.structure:core-must-not-depend-on-renderer[]

	@DisplayName("Supporting packages must not depend on anything from the outside")
	// tag::arch-rules.structure:supporting-packages-are-dependency-free[]
	@ParameterizedTest
	@ValueSource(strings = { "..core.ast", "..core.utils" })
	void independentSupportPackages(String supportPackage) {
		ArchRule rule = noClasses().that()
			.resideInAPackage(supportPackage)
			.should().dependOnClassesThat(
				resideInAPackage("..core..").and(not(resideInAPackage(supportPackage)))
			);
		rule.check(coreClasses);
	}
	// end::arch-rules.structure:supporting-packages-are-dependency-free[]

	@Test
	void allCallsToSchemaNamesMustUseTheBridge() {

		ArchRule rule = noClasses()
			.that().areNotAssignableFrom(SchemaNamesBridge.class)
			.should()
			.callCodeUnitWhere(targetOwner(assignableTo(SchemaNames.class)));
		rule.check(coreClasses);
	}
}
